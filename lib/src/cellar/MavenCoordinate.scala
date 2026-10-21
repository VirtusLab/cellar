package cellar

import cats.effect.IO
import coursier.{Dependency, Module, ModuleName, Organization, Repository, Resolve, VersionConstraint, Versions}

final case class MavenCoordinate(group: String, artifact: String, version: String):
  def render: String = s"$group:$artifact:$version"

  def toCoursierModule: Module = Module(Organization(group), ModuleName(artifact))

  def toCoursierDependency: Dependency = Dependency(toCoursierModule, VersionConstraint(version))

  def resolveLatest(extraRepos: Seq[Repository] = Seq.empty): IO[MavenCoordinate] =
    if version != "latest" then IO.pure(this)
    else IO.blocking {
      latestRelease(extraRepos).fold(
        throw CellarError.CoordinateNotFound(this, new RuntimeException(s"No versions found for '$group:$artifact'"))
      )(release => copy(version = release))
    }

  /** The newest release of `group:artifact`, whatever `version` says; `None` when none is listed.
    * Blocks while it queries the repositories.
    */
  def latestRelease(extraRepos: Seq[Repository]): Option[String] =
    // `Versions` applies only coursier's own mirror files by default, `Resolve` also the scala-cli
    // and Maven settings mirrors; a lookup that skips a mirror the fetch goes through fails behind
    // a firewall.
    Some(
      Versions()
        .noMirrors
        .addMirrors(Resolve.defaultMirrors*)
        .withModule(toCoursierModule)
        .addRepositories(extraRepos*)
        .run()
        .release0
        .asString
    ).filter(_.nonEmpty)

object MavenCoordinate:
  def parse(raw: String): Either[String, MavenCoordinate] =
    raw.split(":", -1).toList match
      case g :: a :: v :: Nil if g.trim.nonEmpty && a.trim.nonEmpty && v.trim.nonEmpty =>
        Right(MavenCoordinate(g, a, v))
      case _ =>
        Left(
          s"Invalid coordinate '$raw'. Expected format: group:artifact:version (exactly three non-empty segments separated by ':')"
        )
