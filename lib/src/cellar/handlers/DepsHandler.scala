package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import coursierapi.Repository

object DepsHandler:
  def run(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using Console[IO]): IO[ExitCode] =
    val program =
      for
        resolved  <- CoursierResolveClient.resolveDeps(coord, extraRepositories)
        formatted <- IO.blocking(DepsFormatter.format(resolved))
        _         <- Console[IO].println(formatted)
      yield ExitCode.Success

    program.handleErrorWith { case e: Throwable =>
      Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }
