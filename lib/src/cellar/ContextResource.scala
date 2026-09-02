package cellar

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import cellar.CoursierFetchClient.ResolvedClasspath
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
    makeWithSources(ResolvedClasspath(jars, Map.empty), jreClasspath).map((ctx, cp, _) => (ctx, cp))

  def makeWithSources(resolved: ResolvedClasspath, jreClasspath: Classpath)(using
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): Resource[IO, (Context, Classpath, SourceJars)] =
    val jars = resolved.jars
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
          (kept, jarClasspath, dropped) = loaded
          _            <- dropped.traverse_(p =>
                            logger.warn(s"dropped unreadable classpath entry (tasty-query MatchError): $p")
                          )
          classpath    = jreClasspath ++ jarClasspath
          ctx          <- IO.blocking(Context.initialize(classpath))
          sourceJars   <- IO(SourceJars.pair(kept, jarClasspath, resolved.sourcesJars)).flatTap {
                            case Some(_) => IO.unit
                            case None    => logger.warn("classpath entries do not line up with jars; sources unavailable")
                          }
        yield (ctx, classpath, sourceJars.getOrElse(SourceJars.empty))
      }
    }

  /** Reads the classpath, excluding paths that cause `MatchError` in tasty-query
    * (e.g. vendor-injected JRT modules such as the Azul CRS client). Returns the excluded paths
    * alongside the classpath so the caller can report them — dropping an entry silently can turn a
    * present symbol into a "not found".
    */
  private def readClasspathRobust(paths: List[Path], dropped: List[Path] = Nil): (List[Path], Classpath, List[Path]) =
    try (paths, ClasspathLoaders.read(paths.map(_.toNioPath)), dropped)
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
  ): Resource[IO, (Context, Classpath, SourceJars)] =
    Resource.eval(CoursierFetchClient.fetchClasspathWithSources(coord, extraRepositories)).flatMap { resolved =>
      makeWithSources(resolved, jreClasspath).evalMap { (ctx, classpath, sourceJars) =>
        IO.blocking {
          if resolved.jars.nonEmpty then
            val jarEntries = classpath.filter(_.toString.endsWith(".jar"))
            val hasSymbols = jarEntries.exists { entry =>
              try ctx.findSymbolsByClasspathEntry(entry).nonEmpty
              catch case _: Exception => false
            }
            if !hasSymbols then throw CellarError.EmptyArtifact(coord)
          (ctx, classpath, sourceJars)
        }
      }
    }
