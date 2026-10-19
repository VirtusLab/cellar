package cellar.cli

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import cellar.Config
import fs2.io.file.{Files, Path}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Instant
import scala.concurrent.duration.*
import scala.jdk.OptionConverters.*

/**
 * Tells users (usually agents reading stderr) when a newer cellar release exists and how to get
 * it for their install channel. The latest release is looked up at most once per
 * [[checkInterval]] and cached in [[cacheFile]]; failures are silent.
 */
object UpdateCheck:

  private[cli] val releasesPage = "https://github.com/VirtusLab/cellar/releases/latest"
  private[cli] val cacheFile    = TelemetrySubcommand.userCellarDir / "update-check"

  private val checkInterval = 24.hours
  private val fetchTimeout  = 2.seconds

  private[cli] case class Cached(checkedAt: Instant, latest: Option[String])

  /** Runs `command` with the update check alongside it, printing the notice once it completes. */
  def around(command: IO[ExitCode]): IO[ExitCode] =
    val enabled =
      Config.global.updateCheck.enabled &&
        !sys.env.contains("CI") &&
        !BuildInfo.version.contains("SNAPSHOT")
    if !enabled then command
    else
      latestRelease(cacheFile, fetchLatest).start.flatMap { check =>
        command.flatTap { _ =>
          check.joinWithNever
            .flatMap(_.traverse_(latest => IO.whenA(isNewer(BuildInfo.version, latest))(printNotice(latest))))
            .handleError(_ => ())
        }
      }

  /** The latest known release, calling `fetch` only when the cache at `cache` is missing or stale. */
  private[cli] def latestRelease(cache: Path, fetch: IO[Option[String]]): IO[Option[String]] =
    (readCache(cache), IO.realTimeInstant).flatMapN { (cached, now) =>
      cached match
        case Some(c) if !isStale(c, now) => IO.pure(c.latest)
        case _                           =>
          // A failed lookup is cached too, so an offline machine pays the timeout once per interval.
          fetch.timeout(fetchTimeout).attempt
            .map(_.toOption.flatten.orElse(cached.flatMap(_.latest)))
            .flatTap(latest => writeCache(cache, Cached(now, latest)).handleError(_ => ()))
    }

  private def printNotice(latest: String): IO[Unit] =
    // On the JVM the process executable is `java`, which says nothing about how cellar was installed.
    val isNative   = System.getProperty("org.graalvm.nativeimage.imagecode") == "runtime"
    val executable = Option.when(isNative)(ProcessHandle.current().info().command().toScala).flatten
    IO.blocking(System.err.println(
      s"cellar $latest is available (installed: ${BuildInfo.version}). Update with: ${updateCommand(executable)}"
    ))

  /** GitHub redirects `releases/latest` to the newest non-prerelease tag, so no API quota is spent. */
  private def fetchLatest: IO[Option[String]] =
    IO.interruptible {
      val client = HttpClient.newBuilder()
        .followRedirects(HttpClient.Redirect.NEVER)
        .connectTimeout(java.time.Duration.ofMillis(fetchTimeout.toMillis))
        .build()
      val request = HttpRequest.newBuilder(URI.create(releasesPage))
        .method("HEAD", HttpRequest.BodyPublishers.noBody())
        .build()
      client.send(request, HttpResponse.BodyHandlers.discarding()).headers().firstValue("location").toScala
    }.map(_.flatMap(versionFromLocation))

  private[cli] def versionFromLocation(location: String): Option[String] =
    """/releases/tag/v?([^/]+)$""".r.findFirstMatchIn(location).map(_.group(1))

  private[cli] def isNewer(current: String, latest: String): Boolean =
    coursier.version.Version(current) < coursier.version.Version(latest)

  private[cli] def isStale(cached: Cached, now: Instant): Boolean =
    cached.checkedAt.plusMillis(checkInterval.toMillis).isBefore(now)

  /** The upgrade instruction matching how the running binary was installed. */
  private[cli] def updateCommand(executable: Option[String]): String =
    executable match
      case Some(p) if p.startsWith("/nix/store/")        => "nix profile upgrade cellar"
      case Some(p) if p.toLowerCase.contains("coursier") => "cs update cellar"
      case _                                             => s"download it from $releasesPage"

  private[cli] def parseCache(content: String): Option[Cached] =
    content.linesIterator.toList match
      case millis :: rest =>
        millis.trim.toLongOption.map(m => Cached(Instant.ofEpochMilli(m), rest.headOption.map(_.trim).filter(_.nonEmpty)))
      case Nil => None

  private[cli] def renderCache(cached: Cached): String =
    s"${cached.checkedAt.toEpochMilli}\n${cached.latest.getOrElse("")}\n"

  private def readCache(cache: Path): IO[Option[Cached]] =
    Files[IO].readUtf8(cache).compile.string.map(parseCache).handleError(_ => None)

  private def writeCache(cache: Path, cached: Cached): IO[Unit] =
    Files[IO].createDirectories(cache.parent.get) *>
      fs2.Stream.emit(renderCache(cached)).through(Files[IO].writeUtf8(cache)).compile.drain
