package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import coursierapi.Repository

object MetaHandler:
  def run(
      coord: MavenCoordinate,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using Console[IO]): IO[ExitCode] =
    val program =
      for
        pomPath  <- CoursierFetchClient.fetchPom(coord, extraRepositories)
        path     <- IO.fromOption(pomPath)(CellarError.CoordinateNotFound(coord, new RuntimeException("POM not found")))
        meta     <- IO.blocking(PomParser.parse(path, coord))
        formatted = MetaFormatter.format(meta)
        _        <- Console[IO].println(formatted)
      yield ExitCode.Success

    program.handleErrorWith { e =>
      Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }
