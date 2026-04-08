package cellar

import cats.effect.IO
import coursierapi.{Cache, Fetch, Repository}
import fs2.io.file.Path

import scala.jdk.CollectionConverters.*

object CoursierFetchClient:
  def fetchSourcesJar(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  ): IO[Option[Path]] =
    IO.blocking {
      val dep   = coord.toCoursierDependency.withTransitive(false)
      val fetch = Fetch.create()
        .addDependencies(dep)
        .withCache(Cache.create())
        .addClassifiers("sources")
        .withMainArtifacts(false)
      if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*)
      fetch.fetch().asScala.headOption.map(file => Path.fromNioPath(file.toPath))
    }.handleError(_ => None)

  def fetchClasspath(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  ): IO[Seq[Path]] =
    IO.blocking {
      val dep   = coord.toCoursierDependency
      val fetch = Fetch.create().addDependencies(dep).withCache(Cache.create())
      if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*)
      fetch.fetch().asScala.toSeq.map(file => Path.fromNioPath(file.toPath))
    }.handleErrorWith { case e: coursierapi.error.CoursierError =>
      CoordinateCompleter.suggest(coord, extraRepositories).flatMap { suggestions =>
        IO.raiseError(CellarError.CoordinateNotFound(coord, e, suggestions))
      }
    }
