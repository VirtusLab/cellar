package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Contexts.Context

object ProjectGetHandler:
  def run(
      fqn: String,
      module: Option[String],
      javaHome: Option[Path] = None,
      noCache: Boolean = false,
      limit: Option[Int] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false,
      cwd: Option[Path] = None,
      config: Config = Config.global,
      testScope: Boolean = false,
      logger: Logger[IO] = StderrLogger.off
  )(using Console[IO], Tracer[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    ProjectHandler.run(javaHome, cwd, module, noCache, config, testScope) { (ctx, classpath, _) =>
      given Context = ctx
      GetHandler.runCore(fqn, classpath, coord = None, limit, hideInherited, groupInherited, logger)
    }
