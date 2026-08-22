package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object ProjectHandler:
  def run(
      javaHome: Option[Path],
      cwd: Option[Path],
      module: Option[String],
      noCache: Boolean,
      config: Config = Config.global,
      testScope: Boolean = false
  )(body: (Context, Classpath, Classpath) => IO[ExitCode])(using
      console: Console[IO],
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): IO[ExitCode] =
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        workingDir   = cwd.getOrElse(Path(System.getProperty("user.dir")))
        result       <- build.ProjectClasspathProvider.provide(workingDir, module, jreClasspath, noCache, config, testScope).use { (ctx, classpath) =>
          body(ctx, classpath, jreClasspath)
        }
      yield result

    program
