package cellar.bench

import cats.effect.{ExitCode, IO, IOApp}

/** `./mill signatureBench.run <language> [coordinate ...]`, printing a Markdown report. */
object Main extends IOApp:
  private val languages = List(JavaLanguage, ScalaLanguage.scala2, ScalaLanguage.scala3).map(l => l.name.toLowerCase -> l).toMap

  def run(args: List[String]): IO[ExitCode] =
    args match
      case lang :: coords if languages.contains(lang.toLowerCase) =>
        val language = languages(lang.toLowerCase)
        Bench
          .run(language, if coords.isEmpty then language.defaultLibraries else coords)
          .flatMap(results => IO.println(Report.render(language, results)))
          .as(ExitCode.Success)
      case _ =>
        IO.println(s"usage: signatureBench.run <${languages.keys.mkString("|")}> [group:artifact:version ...]")
          .as(ExitCode.Error)
