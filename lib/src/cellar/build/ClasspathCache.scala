package cellar.build

import cats.effect.IO
import java.nio.file.{Files, Path, StandardCopyOption}

class ClasspathCache(projectDir: Path):
  private val cacheDir = projectDir.resolve(".cellar").resolve("cache")

  def get(hash: String): IO[Option[List[Path]]] =
    IO.blocking {
      val file = cacheDir.resolve(s"$hash.txt")
      if !Files.exists(file) then None
      else
        val paths = Files.readString(file).linesIterator
          .filter(_.nonEmpty)
          .map(Path.of(_))
          .toList
        // Validate all paths still exist
        if paths.forall(Files.exists(_)) then Some(paths)
        else None
    }

  def put(hash: String, paths: List[Path]): IO[Unit] =
    IO.blocking {
      Files.createDirectories(cacheDir)
      val file = cacheDir.resolve(s"$hash.txt")
      val tmp = cacheDir.resolve(s"$hash.tmp")
      Files.writeString(tmp, paths.map(_.toString).mkString("\n") + "\n")
      Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    }
