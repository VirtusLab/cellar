package cellar.build

import cats.effect.IO
import cellar.CellarError
import cellar.process.ProcessRunner
import fs2.io.file.Path

class ScalaCliBuildTool(cwd: Path) extends BuildTool:
  def kind: BuildToolKind = BuildToolKind.ScalaCli

  def supportsTestScope: Boolean = true

  def compile(module: Option[String], testScope: Boolean): IO[Unit] =
    rejectModule(module) >>
      ProcessRunner.run("scala-cli", List("compile") ::: testFlag(testScope) ::: List("."), Some(cwd)).flatMap { result =>
        if result.exitCode == 0 then IO.unit
        else IO.raiseError(CellarError.CompilationFailed(BuildToolKind.ScalaCli, result.stderr))
      }

  def extractClasspath(module: Option[String], testScope: Boolean): IO[List[Path]] =
    rejectModule(module) >>
      ProcessRunner.run("scala-cli", List("compile", "--print-classpath") ::: testFlag(testScope) ::: List("."), Some(cwd)).flatMap { result =>
        if result.exitCode != 0 then
          IO.raiseError(CellarError.CompilationFailed(BuildToolKind.ScalaCli, result.stderr))
        else
          ClasspathOutputParser.parseColonSeparated(result.stdout) match
            case Left(err)    => IO.raiseError(CellarError.ClasspathExtractionFailed(BuildToolKind.ScalaCli, err))
            case Right(paths) => IO.pure(paths)
      }

  def fingerprintFiles: IO[List[Path]] = IO.pure(Nil)

  private def testFlag(testScope: Boolean): List[String] =
    if testScope then List("--test") else Nil

  private def rejectModule(module: Option[String]): IO[Unit] =
    module match
      case Some(_) => IO.raiseError(CellarError.ModuleNotSupported(BuildToolKind.ScalaCli))
      case None    => IO.unit
