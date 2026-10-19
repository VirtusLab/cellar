package cellar.bench

import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.{Files, Path}

/** `./mill signatureBench.run <language> [--json out.json] [coordinate ...]` prints a Markdown report,
  * and with `--json` also writes every compared pair. `./mill signatureBench.run diff before.json
  * after.json report.html` renders the signatures whose printing changed between two such runs.
  */
object Main extends IOApp:
  private val languages = List(JavaLanguage, ScalaLanguage.scala2, ScalaLanguage.scala3).map(l => l.name.toLowerCase -> l).toMap

  private val usage =
    s"""usage: signatureBench.run <${languages.keys.mkString("|")}> [--json out.json] [group:artifact:version ...]
       |       signatureBench.run diff <before.json> <after.json> <report.html>""".stripMargin

  def run(args: List[String]): IO[ExitCode] =
    args match
      case "diff" :: before :: after :: out :: Nil => diff(Path(before), Path(after), Path(out))
      case lang :: rest if languages.contains(lang.toLowerCase) =>
        val language = languages(lang.toLowerCase)
        val (json, coords) = rest match
          case "--json" :: path :: tail => (Some(Path(path)), tail)
          case other                    => (None, other)
        for
          results <- Bench.run(language, if coords.isEmpty then language.defaultLibraries else coords)
          _       <- IO.println(Report.render(language, results))
          _       <- json.fold(IO.unit)(JsonReport.write(language, results, _))
        yield ExitCode.Success
      case _ => IO.println(usage).as(ExitCode.Error)

  private def diff(before: Path, after: Path, out: Path): IO[ExitCode] =
    for
      was <- JsonReport.read(before)
      now <- JsonReport.read(after)
      code <-
        if was.language != now.language then
          IO.println(s"cannot diff a ${was.language} run against a ${now.language} run").as(ExitCode.Error)
        else
          val result = DiffReport.diff(was, now)
          val html   = DiffReport.html(result, before.fileName.toString, after.fileName.toString)
          fs2.Stream.emit(html).through(Files[IO].writeUtf8(out)).compile.drain *>
            IO.println(DiffReport.summary(result)).as(ExitCode.Success)
    yield code
