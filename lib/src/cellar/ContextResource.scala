package cellar

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import coursierapi.Repository
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders

object ContextResource:
  def make(jars: Seq[Path], jreClasspath: Classpath)(using
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): Resource[IO, (Context, Classpath)] =
    Resource.eval {
      tracer.span("tasty.context.init").surround {
        for
          _            <- logger.debug(s"loading ${jars.size} jar(s)")
          _            <- jars.traverse_(j => logger.debug(s"  $j"))
          loaded       <- IO.blocking(readClasspathRobust(jars.toList)).adaptError { case e =>
                            new RuntimeException(
                              s"Failed to load classpath (${e.getClass.getSimpleName}: ${e.getMessage}). " +
                                "If JRE paths are invalid, set JAVA_HOME or use --java-home.",
                              e
                            )
                          }
          (jarClasspath, dropped) = loaded
          _            <- dropped.traverse_(p =>
                            logger.warn(s"dropped unreadable classpath entry (tasty-query MatchError): $p")
                          )
          classpath    = jreClasspath ++ jarClasspath
          ctx          <- IO.blocking(Context.initialize(classpath))
          _             = JavaParamNames.register(ctx, classpath)
        yield (ctx, classpath)
      }
    }

  /** Reads the classpath, excluding paths that cause `MatchError` in tasty-query
    * (e.g. vendor-injected JRT modules such as the Azul CRS client). Returns the excluded paths
    * alongside the classpath so the caller can report them — dropping an entry silently can turn a
    * present symbol into a "not found".
    */
  private def readClasspathRobust(paths: List[Path], dropped: List[Path] = Nil): (Classpath, List[Path]) =
    try (ClasspathLoaders.read(paths.map(_.toNioPath)), dropped)
    catch
      case e: MatchError =>
        val bad = paths.find { p =>
          try { ClasspathLoaders.read(List(p.toNioPath)): Unit; false }
          catch case _: MatchError => true
        }
        bad match
          case Some(offender) => readClasspathRobust(paths.filterNot(_ == offender), offender :: dropped)
          case None           => throw e

  def makeFromCoord(
      coord: MavenCoordinate,
      jreClasspath: Classpath,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): Resource[IO, (Context, Classpath)] =
    Resource.eval(CoursierFetchClient.fetchClasspath(coord, extraRepositories)).flatMap { jars =>
      make(jars, jreClasspath).evalMap { (ctx, classpath) =>
        IO.blocking {
          if jars.nonEmpty then
            val jarEntries = classpath.filter(_.toString.endsWith(".jar"))
            val hasSymbols = jarEntries.exists { entry =>
              try ctx.findSymbolsByClasspathEntry(entry).nonEmpty
              catch case _: Exception => false
            }
            if !hasSymbols then throw CellarError.EmptyArtifact(coord)
          (ctx, classpath)
        }
      }
    }
