package cellar.build

import cats.effect.IO
import cellar.CellarError
import cellar.process.ProcessRunner
import java.nio.file.{Files, Path}

class MillBuildTool(cwd: Path, binary: String = "./mill") extends BuildTool:
  def kind: BuildToolKind = BuildToolKind.Mill

  def compile(module: Option[String]): IO[Unit] =
    requireModule(module).flatMap { mod =>
      ProcessRunner.run(List(binary, s"$mod.compile"), Some(cwd)).flatMap { result =>
        if result.exitCode == 0 then IO.unit
        else IO.raiseError(CellarError.CompilationFailed(BuildToolKind.Mill, result.stderr))
      }
    }

  def extractClasspath(module: Option[String]): IO[List[Path]] =
    requireModule(module).flatMap { mod =>
      for
        compileResult <- ProcessRunner.run(List(binary, "show", s"$mod.compile"), Some(cwd))
        _ <- checkCompileResult(compileResult, mod)
        classesDir <- IO.blocking(parseClassesDir(compileResult.stdout))
        cpResult <- ProcessRunner.run(List(binary, "show", s"$mod.compileClasspath"), Some(cwd))
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
      IO.blocking(ClasspathOutputParser.parseJsonArray(result.stdout)).flatMap {
        case Left(err)    => IO.raiseError(CellarError.ClasspathExtractionFailed(BuildToolKind.Mill, err))
        case Right(paths) => IO.pure(classesDir.toList ++ paths)
      }

  private def parseClassesDir(stdout: String): Option[Path] =
    val marker = "\"classes\""
    stdout.indexOf(marker) match
      case -1 => None
      case idx =>
        val afterColon = stdout.indexOf(':', idx + marker.length)
        if afterColon == -1 then None
        else
          val valueStart = stdout.indexOf('"', afterColon + 1)
          val valueEnd = stdout.indexOf('"', valueStart + 1)
          if valueStart == -1 || valueEnd == -1 then None
          else
            val raw = stdout.substring(valueStart + 1, valueEnd)
            // Handle ref: or qref: prefixed paths by finding ":/" before the absolute path
            val path = raw.lastIndexOf(":/") match
              case -1  => raw
              case i   => raw.substring(i + 1)
            Some(Path.of(path)).filter(Files.isDirectory(_))

  def fingerprintFiles(): IO[List[Path]] =
    IO.blocking {
      if Files.isDirectory(cwd.resolve(".git")) then fingerprintFromGit()
      else fingerprintFromDisk()
    }

  private def fingerprintFromGit(): List[Path] =
    val patterns = List("build.mill", "build.sc", "build.mill.yaml", "build.yaml", "mill-build/**", ".mill-version")
    val result = new ProcessBuilder(("git" :: "ls-files" :: patterns)*)
      .directory(cwd.toFile)
      .start()
    val stdout = new String(result.getInputStream.readAllBytes())
    if result.waitFor() == 0 then
      stdout.linesIterator.filter(_.nonEmpty).map(cwd.resolve).toList
    else
      fingerprintFromDisk()

  private def fingerprintFromDisk(): List[Path] =
    val candidates = List("build.mill", "build.sc", "build.mill.yaml", "build.yaml", ".mill-version")
    val files = candidates.map(cwd.resolve).filter(Files.exists(_))
    val millBuildDir = cwd.resolve("mill-build")
    val millBuildFiles =
      if Files.isDirectory(millBuildDir) then
        val stream = Files.list(millBuildDir)
        try stream.toArray.map(_.asInstanceOf[Path]).toList
        finally stream.close()
      else Nil
    files ++ millBuildFiles
