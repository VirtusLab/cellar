package cellar.handlers

import cats.effect.std.Console
import cats.effect.{ExitCode, IO}
import cellar.*
import coursierapi.Repository
import org.typelevel.log4cats.Logger

object MetaHandler:
  def run(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty,
      logger: Logger[IO] = StderrLogger.off
  )(using Console[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    val program =
      for
        pomPath  <- CoursierFetchClient.fetchPom(coord, extraRepositories)
        path     <- IO.fromOption(pomPath)(CellarError.CoordinateNotFound(coord, new RuntimeException("POM not found")))
        meta     <- IO.blocking(PomParser.parse(path, coord))
        formatted = MetaFormatter.format(meta)
        _        <- Console[IO].println(formatted)
      yield ExitCode.Success

    program
