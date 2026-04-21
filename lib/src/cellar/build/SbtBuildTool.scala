package cellar.build

import cats.effect.IO
import cats.syntax.all._
import cellar.{CellarError, SbtConfig}
import cellar.process.ProcessRunner

import fs2.io.file.{Files, Path}

class SbtBuildTool(cwd: Path, config: SbtConfig) extends BuildTool:
  def kind: BuildToolKind = BuildToolKind.Sbt

  def compile(module: Option[String]): IO[Unit] =
    requireModule(module).flatMap { mod =>
      ProcessRunner.run(config.binary, config.effectiveExtraArgs ::: List(s"$mod/compile"), Some(cwd)).flatMap { result =>
        IO.raiseUnless(result.exitCode == 0)(
          CellarError.CompilationFailed(BuildToolKind.Sbt, extractErrors(result.stdout, result.stderr))
        )
      }
    }

  def extractClasspath(module: Option[String]): IO[List[Path]] =
    requireModule(module).flatMap { mod =>
      ProcessRunner.run(config.binary, config.effectiveExtraArgs ::: List(s"export $mod/Compile/fullClasspath"), Some(cwd)).flatMap { result =>
        if result.exitCode != 0 then
          IO.raiseError(CellarError.CompilationFailed(BuildToolKind.Sbt, extractErrors(result.stdout, result.stderr)))
        else
          val classpathLine = result.stdout.linesIterator
            .filter(_.nonEmpty)
            .filter(line => !line.startsWith("["))
            .filter(_.contains(java.io.File.separator))
            .toList
            .sortBy(-_.length)
            .headOption

          classpathLine match
            case None =>
              IO.raiseError(CellarError.ClasspathExtractionFailed(BuildToolKind.Sbt, "no classpath line found in output."))
            case Some(line) =>
              ClasspathOutputParser.parseColonSeparated(line) match
                case Left(err)    => IO.raiseError(CellarError.ClasspathExtractionFailed(BuildToolKind.Sbt, err))
                case Right(paths) => IO.pure(paths)
      }
    }

  def fingerprintFiles: IO[List[Path]] =
    val gitPatterns = List("build.sbt", "project/*.sbt", "project/*.scala", "project/build.properties")
    gitOrDiskFingerprint(cwd, gitPatterns, fingerprintFromDisk)

  private def fingerprintFromDisk: IO[List[Path]] =
    val candidates = List("build.sbt", "project/build.properties", "project/plugins.sbt")
    val files = candidates.map(cwd.resolve).filterA(Files[IO].exists(_))
    val projectDir = cwd.resolve("project")
    val projectFiles =
      Files[IO].isDirectory(projectDir).ifM(
        ifTrue = Files[IO].list(projectDir, "*.{sbt,scala}").compile.toList,
        ifFalse = IO.pure(Nil)
      )
    (files, projectFiles).parMapN((f, pf) => (f ++ pf).distinct)

  private def extractErrors(stdout: String, stderr: String): String =
    val errorLines = stdout.linesIterator.filter(_.startsWith("[error]")).mkString("\n")
    if errorLines.nonEmpty then errorLines else stderr
