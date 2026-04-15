package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import fs2.io.file.Path
import tastyquery.Contexts.Context

object ProjectListHandler:
  def run(
      fqn: String,
      module: Option[String],
      limit: Int,
      javaHome: Option[Path] = None,
      noCache: Boolean = false,
      cwd: Option[Path] = None,
      config: Config = Config.global
  )(using Console[IO]): IO[ExitCode] =
    ProjectHandler.run(javaHome, cwd, module, noCache, config) { (ctx, _, _) =>
      given Context = ctx
      ListHandler.runCore(fqn, limit, coord = None)
    }
