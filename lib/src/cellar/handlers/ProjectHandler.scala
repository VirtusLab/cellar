package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import java.nio.file.Path
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

/** Shared infrastructure for project-aware handlers. */
object ProjectHandler:
  def run(
      javaHome: Option[Path],
      cwd: Option[Path],
      module: Option[String],
      noCache: Boolean,
      millBinary: String
  )(body: (Context, Classpath, Classpath) => IO[ExitCode])(using Console[IO]): IO[ExitCode] =
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        workingDir   <- cwd.fold(IO.blocking(Path.of(System.getProperty("user.dir"))))(IO.pure)
        result       <- build.ProjectClasspathProvider.provide(workingDir, module, jreClasspath, noCache, millBinary).use { (ctx, classpath) =>
          body(ctx, classpath, jreClasspath)
        }
      yield result

    program.handleErrorWith { case e: Throwable =>
      Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }
