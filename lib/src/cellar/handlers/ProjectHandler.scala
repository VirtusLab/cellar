package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import java.nio.file.Path
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object ProjectHandler:
  def run(
      javaHome: Option[Path],
      cwd: Option[Path],
      module: Option[String],
      noCache: Boolean,
      config: Config
  )(body: (Context, Classpath, Classpath) => IO[ExitCode])(using Console[IO]): IO[ExitCode] =
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        workingDir   <- cwd.fold(IO.blocking(Path.of(System.getProperty("user.dir"))))(IO.pure)
        result       <- build.ProjectClasspathProvider.provide(workingDir, module, jreClasspath, noCache, config).use { (ctx, classpath) =>
          body(ctx, classpath, jreClasspath)
        }
      yield result

    program.handleErrorWith { case e: Throwable =>
      Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }
