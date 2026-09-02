package cellar

import cats.effect.IO
import coursierapi.{Cache, Fetch, Repository}
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer

import scala.jdk.CollectionConverters.*

object CoursierFetchClient:
  private def logAttempt(coord: MavenCoordinate, what: String, extraRepositories: Seq[Repository])(using
      logger: Logger[IO]
  ): IO[Unit] =
    val repos = if extraRepositories.isEmpty then "default repositories" else extraRepositories.mkString(", ")
    logger.debug(s"$what ${coord.render} from $repos")

  def fetchPom(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using logger: Logger[IO] = StderrLogger.off): IO[Option[Path]] =
    logAttempt(coord, "fetching POM for", extraRepositories) *>
      IO.blocking {
        val dep   = coord.toCoursierDependency.withTransitive(false)
        val fetch = Fetch.create().addDependencies(dep).withCache(Cache.create())
        if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*): Unit
        fetch.fetch().asScala.headOption.map(file => Path.fromNioPath(file.toPath))
      }.flatMap {
        // Coursier always downloads the POM alongside the JAR in the cache; derive its path
        case Some(jar) =>
          val pom = jar.parent.get / (jar.fileName.toString.stripSuffix(".jar") + ".pom")
          Files[IO].exists(pom).map(Option.when(_)(pom))
        case None => IO.none
      }.handleErrorWith {
        case e: coursierapi.error.CoursierError =>
          CoordinateCompleter.suggest(coord, extraRepositories).flatMap { suggestions =>
            IO.raiseError(CellarError.CoordinateNotFound(coord, e, suggestions))
          }
        case e => IO.raiseError(e)
      }

  /** The main jars of `coord`'s transitive closure, each paired with its `-sources.jar` when the
    * publisher shipped one. Coursier treats classifier artifacts as optional, so a dependency
    * without sources costs nothing but its absence from `sourcesJars`.
    */
  case class ResolvedClasspath(jars: Seq[Path], sourcesJars: Map[Path, Path])

  def fetchClasspath(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using tracer: Tracer[IO], logger: Logger[IO] = StderrLogger.off): IO[Seq[Path]] =
    fetchClasspathWithSources(coord, extraRepositories).map(_.jars)

  def fetchClasspathWithSources(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using tracer: Tracer[IO], logger: Logger[IO] = StderrLogger.off): IO[ResolvedClasspath] =
    tracer.span("coursier.fetch").surround {
      logAttempt(coord, "resolving", extraRepositories) *>
        IO.blocking {
          val dep   = coord.toCoursierDependency
          val fetch = Fetch.create()
            .addDependencies(dep)
            .withCache(Cache.create())
            .addClassifiers("sources")
            .withMainArtifacts(true)
          if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*): Unit
          val files            = fetch.fetch().asScala.toSeq.map(file => Path.fromNioPath(file.toPath))
          val (sources, jars)  = files.partition(_.fileName.toString.endsWith(SourcesSuffix))
          val sourcesByStem    = sources.map(p => p.fileName.toString.stripSuffix(SourcesSuffix) -> p).toMap
          val paired = jars.flatMap { jar =>
            sourcesByStem.get(jar.fileName.toString.stripSuffix(".jar")).map(jar -> _)
          }
          ResolvedClasspath(jars, paired.toMap)
        }.handleErrorWith { case e: coursierapi.error.CoursierError =>
          CoordinateCompleter.suggest(coord, extraRepositories).flatMap { suggestions =>
            IO.raiseError(CellarError.CoordinateNotFound(coord, e, suggestions))
          }
        }
    }

  private val SourcesSuffix = "-sources.jar"
