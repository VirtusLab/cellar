package cellar.build

import cats.effect.IO
import cats.syntax.all._
import cellar.{CellarError, MillConfig}
import cellar.process.ProcessRunner

import fs2.io.file.{Files, Path}

class MillBuildTool(cwd: Path, config: MillConfig) extends BuildTool:
  def kind: BuildToolKind = BuildToolKind.Mill

  def compile(module: Option[String]): IO[Unit] =
    requireModule(module).flatMap { mod =>
      ProcessRunner.run(config.binary, List(s"$mod.compile"), Some(cwd)).flatMap { result =>
        if result.exitCode == 0 then IO.unit
        else IO.raiseError(CellarError.CompilationFailed(BuildToolKind.Mill, result.stderr))
      }
    }

  def extractClasspath(module: Option[String]): IO[List[Path]] =
    requireModule(module).flatMap { mod =>
      for
        compileResult <- ProcessRunner.run(config.binary, List("show", s"$mod.compile"), Some(cwd))
        _ <- checkCompileResult(compileResult, mod)
        classesDir <- parseClassesDir(compileResult.stdout)
        cpResult <- ProcessRunner.run(config.binary, List("show", s"$mod.compileClasspath"), Some(cwd))
        paths <- parseClasspathResult(cpResult, classesDir)
      yield paths
    }

  private def checkCompileResult(result: cellar.process.ProcessResult, mod: String): IO[Unit] =
    if result.exitCode == 0 then IO.unit
    else if result.stderr.contains("not found") || result.stderr.contains("Cannot resolve") then
      IO.raiseError(CellarError.ModuleNotFound(BuildToolKind.Mill, mod))
    else
      IO.raiseError(CellarError.CompilationFailed(BuildToolKind.Mill, result.stderr))

  private def parseClasspathResult(result: cellar.process.ProcessResult, classesDir: Option[Path]): IO[List[Path]] =
    if result.exitCode != 0 then
      IO.raiseError(CellarError.ClasspathExtractionFailed(BuildToolKind.Mill, result.stderr))
    else
      ClasspathOutputParser.parseJsonArray(result.stdout).flatMap {
        case Left(err)    => IO.raiseError(CellarError.ClasspathExtractionFailed(BuildToolKind.Mill, err))
        case Right(paths) => IO.pure(classesDir.toList ++ paths)
      }

  private def parseClassesDir(stdout: String): IO[Option[Path]] =
    val marker = "\"classes\""
    stdout.indexOf(marker) match
      case -1 => IO.pure(None)
      case idx =>
        val afterColon = stdout.indexOf(':', idx + marker.length)
        if afterColon == -1 then IO.pure(None)
        else
          val valueStart = stdout.indexOf('"', afterColon + 1)
          val valueEnd = stdout.indexOf('"', valueStart + 1)
          if valueStart == -1 || valueEnd == -1 then IO.pure(None)
          else
            val raw = stdout.substring(valueStart + 1, valueEnd)
            // Handle ref: or qref: prefixed paths by finding ":/" before the absolute path
            val path = raw.lastIndexOf(":/") match
              case -1  => raw
              case i   => raw.substring(i + 1)
            Path(path).some.filterA(Files[IO].isDirectory(_))

  def fingerprintFiles: IO[List[Path]] =
    Files[IO].isDirectory(cwd.resolve(".git")).ifM(
      ifTrue = fingerprintFromGit,
      ifFalse = fingerprintFromDisk
    )

  private def fingerprintFromGit: IO[List[Path]] =
    val patterns = List("build.mill", "build.sc", "build.mill.yaml", "build.yaml", "mill-build/**", ".mill-version")
    ProcessRunner.run("git", "ls-files" :: patterns, Some(cwd)).flatMap { result =>
      if result.exitCode == 0 then
        IO.pure(result.stdout.linesIterator.filter(_.nonEmpty).map(cwd.resolve).toList)
      else
        fingerprintFromDisk
    }

  private def fingerprintFromDisk: IO[List[Path]] =
    val candidates = List("build.mill", "build.sc", "build.mill.yaml", "build.yaml", ".mill-version")
    val files = candidates.map(cwd.resolve).filterA(Files[IO].exists(_))
    val millBuildDir = cwd.resolve("mill-build")
    val millBuildFiles = Files[IO].isDirectory(millBuildDir).flatMap {
        case true  => Files[IO].list(millBuildDir).compile.toList
        case false => IO.pure(Nil)
    }
    (files, millBuildFiles).parMapN(_ ++ _)
