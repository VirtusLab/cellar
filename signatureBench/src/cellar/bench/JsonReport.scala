package cellar.bench

import cats.effect.IO
import fs2.io.file.{Files, Path}

/** Every compared pair of one benchmark run, so two runs can be diffed with [[DiffReport]]. */
object JsonReport:
  final case class Entry(library: String, pair: Pair)
  final case class Run(language: String, entries: List[Entry])

  def write(language: Language, results: List[LibraryResult], path: Path): IO[Unit] =
    val json = ujson.Obj(
      "language" -> language.name,
      "libraries" -> results.map { r =>
        ujson.Obj(
          "coordinate" -> r.coordinate,
          "pairs" -> r.pairs.map { p =>
            ujson.Obj(
              "owner"    -> p.owner,
              "key"      -> p.key,
              "source"   -> p.source,
              "printed"  -> p.printed,
              "category" -> p.category.fold(ujson.Null)(ujson.Str(_))
            )
          }
        )
      }
    )
    fs2.Stream.emit(ujson.write(json)).through(Files[IO].writeUtf8(path)).compile.drain

  def read(path: Path): IO[Run] =
    Files[IO].readUtf8(path).compile.string.map(text => parse(ujson.read(text)))

  private def parse(json: ujson.Value): Run =
    val entries = json("libraries").arr.toList.flatMap { lib =>
      lib("pairs").arr.toList.map { p =>
        Entry(
          lib("coordinate").str,
          Pair(p("owner").str, p("key").str, p("source").str, p("printed").str, p("category").strOpt)
        )
      }
    }
    Run(json("language").str, entries)
