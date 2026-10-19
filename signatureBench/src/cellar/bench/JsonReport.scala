package cellar.bench

import cats.effect.IO
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import fs2.io.file.{Files, Path}

/** Every compared pair of one benchmark run, so two runs can be diffed with [[DiffReport]]. */
object JsonReport:
  final case class Entry(library: String, pair: Pair)
  final case class Run(language: String, entries: List[Entry])

  private final case class LibraryFile(coordinate: String, pairs: List[Pair])
  private final case class RunFile(language: String, libraries: List[LibraryFile])

  // jsoniter leaves empty collections out by default, which readers of the file would have to guess.
  private given JsonValueCodec[RunFile] = JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))

  def write(language: Language, results: List[LibraryResult], path: Path): IO[Unit] =
    val file = RunFile(language.name, results.map(r => LibraryFile(r.coordinate, r.pairs)))
    fs2.Stream.emit(writeToString(file)).through(Files[IO].writeUtf8(path)).compile.drain

  def read(path: Path): IO[Run] =
    Files[IO].readUtf8(path).compile.string.map { text =>
      val file = readFromString[RunFile](text)
      Run(file.language, file.libraries.flatMap(lib => lib.pairs.map(Entry(lib.coordinate, _))))
    }
