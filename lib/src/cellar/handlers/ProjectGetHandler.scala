package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import java.nio.file.Path
import tastyquery.Contexts.Context

object ProjectGetHandler:
  def run(
      fqn: String,
      module: Option[String],
      javaHome: Option[Path] = None,
      noCache: Boolean = false,
      cwd: Option[Path] = None,
      millBinary: String = "./mill"
  )(using Console[IO]): IO[ExitCode] =
    val program =
      for
        jrePaths   <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        workingDir <- cwd.fold(IO.blocking(Path.of(System.getProperty("user.dir"))))(IO.pure)
        result     <- build.ProjectClasspathProvider.provide(workingDir, module, jrePaths, noCache, millBinary).use { (ctx, classpath) =>
          given Context = ctx
          GetHandler.runCore(fqn, classpath, coord = None)
        }
      yield result

    program.handleErrorWith {
      case e: CellarError => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
      case e: Throwable   => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }
