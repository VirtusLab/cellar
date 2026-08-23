package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import coursierapi.Repository
import org.typelevel.log4cats.Logger

object DepsHandler:
  def run(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty,
      logger: Logger[IO] = StderrLogger.off
  )(using Console[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    val program =
      for
        resolved  <- CoursierResolveClient.resolveDeps(coord, extraRepositories)
        formatted <- IO.blocking(DepsFormatter.format(resolved))
        _         <- Console[IO].println(formatted)
      yield ExitCode.Success

    program
