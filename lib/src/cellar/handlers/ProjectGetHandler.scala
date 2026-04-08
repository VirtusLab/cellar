package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import fs2.io.file.Path
import tastyquery.Contexts.Context

object ProjectGetHandler:
  def run(
      fqn: String,
      module: Option[String],
      config: Config,
      javaHome: Option[Path] = None,
      noCache: Boolean = false,
      cwd: Option[Path] = None
  )(using Console[IO]): IO[ExitCode] =
    ProjectHandler.run(javaHome, cwd, module, noCache, config) { (ctx, classpath, _) =>
      given Context = ctx
      GetHandler.runCore(fqn, classpath, coord = None)
    }
