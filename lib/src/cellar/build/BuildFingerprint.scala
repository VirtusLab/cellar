package cellar.build

import cats.effect.IO
import cats.syntax.all.*
import fs2.Chunk
import fs2.io.file.{Files, Path}
import fs2.hashing.*

import java.nio.charset.StandardCharsets

object BuildFingerprint:
  def compute(files: List[Path], module: String): IO[String] =
    Hashing[IO].hasher(HashAlgorithm.SHA256).use { hasher =>
      for
        _ <- hasher.update(Chunk.array(module.getBytes(StandardCharsets.UTF_8)))
        _ <- fs2.Stream.emits(files.sortBy(_.toNioPath))
          .evalFilter(Files[IO].exists)
          .evalTap(path => hasher.update(Chunk.array(path.toString.getBytes(StandardCharsets.UTF_8))))
          .flatMap(path => Files[IO].readAll(path).through(hasher.update))
          .compile.drain
        hash <- hasher.hash
      yield hash.bytes.foldMap(b => f"$b%02x")
    }
