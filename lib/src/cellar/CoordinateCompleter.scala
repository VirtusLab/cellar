package cellar

import cats.effect.IO
import coursier.complete.Complete
import coursier.{Repository, Resolve}

object CoordinateCompleter:
  def suggest(coord: MavenCoordinate, extraRepos: Seq[Repository]): IO[List[String]] =
    IO.blocking {
      def release(artifact: String): Option[String] = coord.copy(artifact = artifact).latestRelease(extraRepos)

      // Completion knows nothing of mirrors; hand it the repositories a fetch actually goes to.
      val resolve     = Resolve().addRepositories(extraRepos*)
      val mirrored    = resolve.finalRepositories.unsafeRun()(using resolve.cache.ec)
      val complete    = Complete().copy(repositories = mirrored, input = s"${coord.group}:${coord.artifact}")
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
