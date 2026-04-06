package cellar.process

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Path
import fs2.io.process.*

final case class ProcessResult(exitCode: Int, stdout: String, stderr: String)

object ProcessRunner:
  def run(command: String, args: List[String], workingDir: Option[Path] = None): IO[ProcessResult] = {
    val builder = ProcessBuilder(command, args)
    val withWorkingDir = workingDir.fold(builder)(dir => builder.withWorkingDirectory(dir))
    withWorkingDir.spawn[IO].use { process =>
      (
        process.exitValue,
        process.stdout.through(fs2.text.utf8.decode).compile.string,
        process.stderr.through(fs2.text.utf8.decode).compile.string
      ).parMapN(ProcessResult.apply)
    }.adaptError { case e: java.io.IOException =>
      val detail = Option(e.getMessage).getOrElse("I/O error")
      new RuntimeException(s"Failed to start '$command': $detail", e)
    }
  }
