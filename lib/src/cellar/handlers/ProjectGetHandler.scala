package cellar.handlers

import cats.effect.std.Console
import cats.effect.{ExitCode, IO}
import cellar.*
import coursier.Repository
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
      hideInherited: Boolean = false,
      groupInherited: Boolean = false,
      cwd: Option[Path] = None,
      config: Config = Config.global,
      testScope: Boolean = false,
      extraRepositories: Seq[Repository] = Seq.empty,
      logger: Logger[IO] = StderrLogger.off
  )(using Console[IO], Tracer[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    ProjectHandler.run(javaHome, cwd, module, noCache, config, testScope, extraRepositories) { (ctx, classpath, _, sourceJars) =>
      given Context = ctx
      GetHandler.runCore(fqn, classpath, coord = None, sourceJars, hideInherited, groupInherited, logger)
    }
