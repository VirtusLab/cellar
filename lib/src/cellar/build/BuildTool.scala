package cellar.build

import cats.effect.IO
import cellar.CellarError
import cellar.process.ProcessRunner
import fs2.io.file.Path

trait BuildTool:
  def kind: BuildToolKind
  def compile(module: Option[String]): IO[Unit]
  def extractClasspath(module: Option[String]): IO[List[Path]]
  def fingerprintFiles: IO[List[Path]]

  protected def requireModule(module: Option[String]): IO[String] =
    module match
      case Some(m) => IO.pure(m)
      case None    => IO.raiseError(CellarError.ModuleRequired(kind))

  protected def gitOrDiskFingerprint(cwd: Path, gitPatterns: List[String], fromDisk: IO[List[Path]]): IO[List[Path]] =
    ProcessRunner.run("git", "ls-files" :: gitPatterns, Some(cwd)).flatMap { result =>
      if result.exitCode == 0 then
        IO.pure(result.stdout.linesIterator.filter(_.nonEmpty).map(cwd.resolve).toList)
      else fromDisk
    }
