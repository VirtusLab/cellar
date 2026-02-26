package cellar

import cats.effect.IO
import coursierapi.{Cache, Dependency, Fetch, Repository}
import scala.jdk.CollectionConverters.*

/** Resolved dependency list — a flat sorted sequence of all transitive dependencies. */
final case class ResolvedDeps(root: MavenCoordinate, deps: Seq[Dependency])

object CoursierResolveClient:
  def resolveDeps(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  ): IO[ResolvedDeps] =
    IO.blocking {
      val dep   = coord.toCoursierDependency
      val fetch = Fetch.create().addDependencies(dep).withCache(Cache.create())
      if extraRepositories.nonEmpty then fetch.addRepositories(extraRepositories*)
      val result = fetch.fetchResult()
      val deps   = result.getDependencies.asScala.toSeq
        .sortBy(d => s"${d.getModule.getOrganization}:${d.getModule.getName}:${d.getVersion}")
      ResolvedDeps(coord, deps)
    }.handleErrorWith { case e: coursierapi.error.FetchError =>
      CoordinateCompleter.suggest(coord, extraRepositories).flatMap { suggestions =>
        IO.raiseError(CellarError.CoordinateNotFound(coord, e, suggestions))
      }
    }
