package cellar

import cats.effect.IO
import coursier.{Dependency, Module, ModuleName, Organization, Repository, VersionConstraint, Versions}

final case class MavenCoordinate(group: String, artifact: String, version: String):
  def render: String = s"$group:$artifact:$version"

  def toCoursierModule: Module = Module(Organization(group), ModuleName(artifact))

  def toCoursierDependency: Dependency = Dependency(toCoursierModule, VersionConstraint(version))

  def resolveLatest(extraRepos: Seq[Repository] = Seq.empty): IO[MavenCoordinate] =
    if version != "latest" then IO.pure(this)
    else IO.blocking {
      val release = Versions(CoursierCache.instance).withModule(toCoursierModule).addRepositories(extraRepos*).run().release0.asString
      if release.nonEmpty then copy(version = release)
      else throw CellarError.CoordinateNotFound(this, new RuntimeException(s"No versions found for '$group:$artifact'"))
    }

object MavenCoordinate:
  def parse(raw: String): Either[String, MavenCoordinate] =
    raw.split(":", -1).toList match
      case g :: a :: v :: Nil if g.trim.nonEmpty && a.trim.nonEmpty && v.trim.nonEmpty =>
        Right(MavenCoordinate(g, a, v))
      case _ =>
        Left(
          s"Invalid coordinate '$raw'. Expected format: group:artifact:version (exactly three non-empty segments separated by ':')"
        )
