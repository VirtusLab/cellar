package cellar

import cats.effect.IO
import coursierapi.{Cache, Fetch, Repository}
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer

import scala.jdk.CollectionConverters.*

object CoursierFetchClient:
  private def logAttempt(coord: MavenCoordinate, what: String, extraRepositories: Seq[Repository])(using
      logger: Logger[IO]
  ): IO[Unit] =
    val repos = if extraRepositories.isEmpty then "default repositories" else extraRepositories.mkString(", ")
    logger.debug(s"$what ${coord.render} from $repos")

  def fetchSourcesJar(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using logger: Logger[IO] = StderrLogger.off): IO[Option[Path]] =
    logAttempt(coord, "fetching sources for", extraRepositories) *>
      IO.blocking {
        val dep   = coord.toCoursierDependency.withTransitive(false)
        val fetch = Fetch.create()
          .addDependencies(dep)
          .withCache(Cache.create())
          .addClassifiers("sources")
          .withMainArtifacts(false)
        if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*): Unit
        fetch.fetch().asScala.headOption.map(file => Path.fromNioPath(file.toPath))
      }.handleErrorWith(e => logger.debug(s"no sources jar for ${coord.render}: ${e.getMessage}").as(None))

  def fetchPom(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using logger: Logger[IO] = StderrLogger.off): IO[Option[Path]] =
    logAttempt(coord, "fetching POM for", extraRepositories) *>
      IO.blocking {
        val dep   = coord.toCoursierDependency.withTransitive(false)
        val fetch = Fetch.create().addDependencies(dep).withCache(Cache.create())
        if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*): Unit
        // Coursier always downloads the POM alongside the JAR in the cache; derive its path
        fetch.fetch().asScala.headOption.map(_.toPath)
          .flatMap { jarNio =>
            val pomNio = jarNio.getParent.resolve(jarNio.getFileName.toString.stripSuffix(".jar") + ".pom")
            Option.when(java.nio.file.Files.exists(pomNio))(Path.fromNioPath(pomNio))
          }
      }.handleErrorWith {
        case e: coursierapi.error.CoursierError =>
          CoordinateCompleter.suggest(coord, extraRepositories).flatMap { suggestions =>
            IO.raiseError(CellarError.CoordinateNotFound(coord, e, suggestions))
          }
        case e => IO.raiseError(e)
      }

  def fetchClasspath(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using tracer: Tracer[IO], logger: Logger[IO] = StderrLogger.off): IO[Seq[Path]] =
    tracer.span("coursier.fetch").surround {
      logAttempt(coord, "resolving", extraRepositories) *>
        IO.blocking {
          val dep   = coord.toCoursierDependency
          val fetch = Fetch.create().addDependencies(dep).withCache(Cache.create())
          if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*): Unit
          fetch.fetch().asScala.toSeq.map(file => Path.fromNioPath(file.toPath))
        }.handleErrorWith { case e: coursierapi.error.CoursierError =>
          CoordinateCompleter.suggest(coord, extraRepositories).flatMap { suggestions =>
            IO.raiseError(CellarError.CoordinateNotFound(coord, e, suggestions))
          }
        }
    }
