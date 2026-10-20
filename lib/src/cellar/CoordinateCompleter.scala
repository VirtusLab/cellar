package cellar

import cats.effect.IO
import coursier.complete.Complete
import coursier.{Repository, Versions}

object CoordinateCompleter:
  def suggest(coord: MavenCoordinate, extraRepos: Seq[Repository]): IO[List[String]] =
    IO.blocking {
      def release(artifact: String): Option[String] =
        Some(Versions(CoursierCache.instance).withModule(coord.copy(artifact = artifact).toCoursierModule).addRepositories(extraRepos*).run())
          .map(_.release0.asString)
          .filter(_.nonEmpty)

      val complete    = Complete(CoursierCache.instance).addRepositories(extraRepos*).copy(input = s"${coord.group}:${coord.artifact}")
      val completions = complete.complete().unsafeRun()(using complete.cache.ec)._2.toList

      val exactMatch = completions.contains(coord.artifact)

      if exactMatch then
        // Artifact exists — must be a version problem
        release(coord.artifact).map(r => s"Artifact exists. Latest version: $r").toList
      else if completions.nonEmpty then
        completions.take(5).map { artifactName =>
          release(artifactName).fold(s"${coord.group}:$artifactName")(r => s"${coord.group}:$artifactName:$r")
        }
      else Nil
    }.handleError(_ => Nil)
