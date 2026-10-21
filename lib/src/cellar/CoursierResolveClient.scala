package cellar

import cats.effect.IO
import coursier.{Dependency, Repository, Resolve}
import org.typelevel.log4cats.Logger

/** Resolved dependency list — a flat sorted sequence of all transitive dependencies. */
final case class ResolvedDeps(root: MavenCoordinate, deps: Seq[Dependency])

object CoursierResolveClient:
  def resolveDeps(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using logger: Logger[IO] = StderrLogger.off): IO[ResolvedDeps] =
    logger.debug(s"resolving dependency tree for ${coord.render}") *>
      IO.blocking {
        val resolution = Resolve()
          .addDependencies(coord.toCoursierDependency)
          .addRepositories(extraRepositories*)
          .run()
        val deps = resolution.orderedDependencies
          .distinctBy(_.moduleVersionConstraint)
          .sortBy(d => s"${d.module.organization.value}:${d.module.name.value}:${d.versionConstraint.asString}")
        ResolvedDeps(coord, deps)
      }.handleErrorWith { case e: coursier.error.CoursierError =>
        CoordinateCompleter.suggest(coord, extraRepositories).flatMap { suggestions =>
          IO.raiseError(CellarError.CoordinateNotFound(coord, e, suggestions))
        }
      }
