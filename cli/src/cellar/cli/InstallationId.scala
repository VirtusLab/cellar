package cellar.cli

import cats.effect.IO
import fs2.io.file.{Files, Path}

import java.util.UUID

object InstallationId:

  private val idFile = Path(sys.props("user.home")) / ".cellar" / "installation_id"

  def read: IO[Option[String]] =
    Files[IO].exists(idFile).flatMap {
      case false => IO.pure(None)
      case true  => Files[IO].readUtf8(idFile).compile.string.map(s => Some(s.trim))
    }

  def ensure: IO[String] =
    read.flatMap {
      case Some(id) => IO.pure(id)
      case None     =>
        val id = UUID.randomUUID().toString
        val dir = idFile.parent.get
        Files[IO].createDirectories(dir) *>
          fs2.Stream.emit(id).through(Files[IO].writeUtf8(idFile)).compile.drain.as(id)
    }
