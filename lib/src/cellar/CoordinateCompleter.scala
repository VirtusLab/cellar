package cellar

import cats.effect.IO
import coursierapi.{Complete, Module, Repository, Versions}
import scala.jdk.CollectionConverters.*

object CoordinateCompleter:
  def suggest(coord: MavenCoordinate, extraRepos: Seq[Repository]): IO[List[String]] =
    IO.blocking {
      val complete = Complete.create().withInput(s"${coord.group}:${coord.artifact}")
      if extraRepos.nonEmpty then complete.addRepositories(extraRepos*)
      val completions = complete.complete().getCompletions.asScala.toList

      val exactMatch = completions.contains(coord.artifact)

      if exactMatch then
        // Artifact exists — must be a version problem
        val versions = Versions.create().withModule(Module.of(coord.group, coord.artifact, java.util.Collections.emptyMap()))
        if extraRepos.nonEmpty then versions.addRepositories(extraRepos*)
        val latest = versions.versions().getMergedListings.getLatest
        if latest != null && latest.nonEmpty then
          List(s"Artifact exists. Latest version: $latest")
        else Nil
      else if completions.nonEmpty then
        completions.take(5).flatMap { artifactName =>
          val module = Module.of(coord.group, artifactName, java.util.Collections.emptyMap())
          val versions = Versions.create().withModule(module)
          if extraRepos.nonEmpty then versions.addRepositories(extraRepos*)
          val latest = versions.versions().getMergedListings.getLatest
          if latest != null && latest.nonEmpty then
            Some(s"${coord.group}:$artifactName:$latest")
          else
            Some(s"${coord.group}:$artifactName")
        }
      else Nil
    }.handleError(_ => Nil)
