package cellar.cli

import cats.effect.IO
import fs2.io.file.Files

import java.util.UUID

object InstallationId:

  private[cli] val idFile = TelemetrySubcommand.userCellarDir / "installation_id"

  def read: IO[Option[String]] =
    Files[IO].readUtf8(idFile).compile.string
      .map(s => Some(s.trim).filter(_.nonEmpty))
      .recover { case _: java.nio.file.NoSuchFileException => None }

  def ensure: IO[String] =
    read.flatMap {
      case Some(id) => IO.pure(id)
      case None     => write(UUID.randomUUID().toString)
    }

  def reset: IO[String] = write(UUID.randomUUID().toString)

  private def write(id: String): IO[String] =
    Files[IO].createDirectories(idFile.parent.get) *>
      fs2.Stream.emit(id).through(Files[IO].writeUtf8(idFile)).compile.drain.as(id)
