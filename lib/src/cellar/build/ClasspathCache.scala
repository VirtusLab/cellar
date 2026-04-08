package cellar.build

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{CopyFlag, CopyFlags, Files, Path}

class ClasspathCache(projectDir: Path):
  private val cacheDir = projectDir.resolve(".cellar").resolve("cache")

  def get(hash: String): IO[Option[List[Path]]] =
    val file = cacheDir.resolve(s"$hash.txt")
    Files[IO].exists(file).flatMap {
      case true =>
        for
          paths <- Files[IO].readUtf8Lines(file).filter(_.nonEmpty).map(Path(_)).compile.toList
          allExist <- paths.forallM(Files[IO].exists)
        yield Option.when(allExist)(paths)
      case false => IO.pure(None)
    }

  def put(hash: String, paths: List[Path]): IO[Unit] = for {
    _ <- Files[IO].createDirectories(cacheDir)
    file = cacheDir.resolve(s"$hash.txt")
    tmp = cacheDir.resolve(s"$hash.tmp")
    _ <- Stream(paths.map(_.toString).mkString("\n") + "\n").through(Files[IO].writeUtf8(tmp)).compile.drain
    _ <- Files[IO].move(tmp, file, CopyFlags(CopyFlag.ReplaceExisting, CopyFlag.AtomicMove))
  } yield ()
