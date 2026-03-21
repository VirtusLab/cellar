package cellar.process

import cats.effect.IO
import cats.syntax.all.*
import java.nio.file.Path

final case class ProcessResult(exitCode: Int, stdout: String, stderr: String)

object ProcessRunner:
  def run(command: List[String], workingDir: Option[Path] = None): IO[ProcessResult] =
    IO.blocking {
      val builder = new ProcessBuilder(command*)
      workingDir.foreach(dir => builder.directory(dir.toFile))
      builder.redirectErrorStream(false)
      val process = builder.start()
      try
        val stdout = new String(process.getInputStream.readAllBytes())
        val stderr = new String(process.getErrorStream.readAllBytes())
        val exitCode = process.waitFor()
        ProcessResult(exitCode, stdout, stderr)
      finally process.destroyForcibly()
    }.onCancel(IO.unit) // process destroyed in finally
      .adaptError { case e: java.io.IOException =>
        new RuntimeException(s"Command not found: '${command.headOption.getOrElse("")}'. Ensure it is installed and on PATH.", e)
      }
