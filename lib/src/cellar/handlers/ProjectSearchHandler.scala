package cellar.handlers

import cats.effect.std.Console
import cats.effect.{ExitCode, IO}
import cellar.*
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Contexts.Context

object ProjectSearchHandler:
  def run(
      query: String,
      module: Option[String],
      limit: Int,
      javaHome: Option[Path] = None,
      noCache: Boolean = false,
      cwd: Option[Path] = None,
      config: Config = Config.global,
      testScope: Boolean = false,
      logger: Logger[IO] = StderrLogger.off
  )(using Console[IO], Tracer[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    ProjectHandler.run(javaHome, cwd, module, noCache, config, testScope) { (ctx, classpath, jreClasspath) =>
      given Context = ctx
      SearchHandler.runCore(query, limit, classpath, jreClasspath)
    }
