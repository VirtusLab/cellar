package cellar

import cats.effect.IO
import coursierapi.{Dependency, Module, Repository, Versions}

final case class MavenCoordinate(group: String, artifact: String, version: String):
  def render: String = s"$group:$artifact:$version"

  def toCoursierDependency: Dependency =
    Dependency.of(Module.of(group, artifact, java.util.Collections.emptyMap()), version)

  def resolveLatest(extraRepos: Seq[Repository] = Seq.empty): IO[MavenCoordinate] =
    if version != "latest" then IO.pure(this)
    else IO.blocking {
      val module = Module.of(group, artifact, java.util.Collections.emptyMap())
      val versions = Versions.create().withModule(module)
      if extraRepos.nonEmpty then versions.addRepositories(extraRepos*): Unit
      val release = versions.versions().getMergedListings.getRelease
      if release != null && release.nonEmpty then copy(version = release)
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
