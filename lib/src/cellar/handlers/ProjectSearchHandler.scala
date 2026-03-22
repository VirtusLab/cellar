package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import java.nio.file.Path
import tastyquery.Contexts.Context

object ProjectSearchHandler:
  def run(
      query: String,
      module: Option[String],
      limit: Int,
      javaHome: Option[Path] = None,
      noCache: Boolean = false,
      cwd: Option[Path] = None,
      millBinary: String = "./mill"
  )(using Console[IO]): IO[ExitCode] =
    ProjectHandler.run(javaHome, cwd, module, noCache, millBinary) { (ctx, classpath, jreClasspath) =>
      given Context = ctx
      SearchHandler.runCore(query, limit, classpath, jreClasspath)
    }
