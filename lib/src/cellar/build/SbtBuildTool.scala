package cellar.build

import cats.effect.IO
import cellar.CellarError
import cellar.process.ProcessRunner
import java.nio.file.{Files, Path}

class SbtBuildTool(cwd: Path) extends BuildTool:
  def name: String = "sbt"

  def compile(module: Option[String]): IO[Unit] =
    requireModule(module).flatMap { mod =>
      ProcessRunner.run(List("sbt", "--batch", s"$mod/compile"), Some(cwd)).flatMap { result =>
        if result.exitCode == 0 then IO.unit
        else IO.raiseError(CellarError.CompilationFailed(BuildToolKind.Sbt, extractErrors(result.stdout, result.stderr)))
      }
    }

  def extractClasspath(module: Option[String]): IO[List[Path]] =
    requireModule(module).flatMap { mod =>
      ProcessRunner.run(List("sbt", "--batch", s"$mod/export compile:fullClasspath"), Some(cwd)).flatMap { result =>
        if result.exitCode != 0 then
          IO.raiseError(CellarError.CompilationFailed(BuildToolKind.Sbt, extractErrors(result.stdout, result.stderr)))
        else
          // Filter sbt logging noise — classpath line doesn't start with [
          val classpathLine = result.stdout.linesIterator
            .filter(_.nonEmpty)
            .filter(line => !line.startsWith("["))
            .filter(_.contains(java.io.File.separator))
            .toList
            .sortBy(-_.length) // longest line is most likely the classpath
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

  def fingerprintFiles(module: Option[String]): IO[List[Path]] =
    IO.blocking {
      val isGit = Files.isDirectory(cwd.resolve(".git"))
      if isGit then fingerprintFromGit()
      else fingerprintFromDisk()
    }

  private def fingerprintFromGit(): List[Path] =
    val patterns = List("build.sbt", "project/*.sbt", "project/*.scala", "project/build.properties")
    val result = new ProcessBuilder(("git" :: "ls-files" :: patterns)*)
      .directory(cwd.toFile)
      .start()
    val stdout = new String(result.getInputStream.readAllBytes())
    result.waitFor()
    stdout.linesIterator.filter(_.nonEmpty).map(cwd.resolve).toList

  private def fingerprintFromDisk(): List[Path] =
    val candidates = List("build.sbt", "project/build.properties", "project/plugins.sbt")
    val files = candidates.map(cwd.resolve).filter(Files.exists(_))
    val projectDir = cwd.resolve("project")
    val projectFiles =
      if Files.isDirectory(projectDir) then
        Files.list(projectDir).toArray.map(_.asInstanceOf[Path]).toList
          .filter(p => p.toString.endsWith(".sbt") || p.toString.endsWith(".scala"))
      else Nil
    (files ++ projectFiles).distinct

  private def requireModule(module: Option[String]): IO[String] =
    module match
      case Some(m) => IO.pure(m)
      case None    => IO.raiseError(CellarError.ModuleRequired(BuildToolKind.Sbt))

  private def extractErrors(stdout: String, stderr: String): String =
    val errorLines = stdout.linesIterator.filter(_.startsWith("[error]")).mkString("\n")
    if errorLines.nonEmpty then errorLines else stderr
