package cellar.build

import cats.effect.{IO, Resource}
import cellar.{Config, ContextResource, StderrLogger}

import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object ProjectClasspathProvider:
  def provide(
      cwd: Path,
      module: Option[String],
      jreClasspath: Classpath,
      noCache: Boolean,
      config: Config = Config.global,
      testScope: Boolean = false
  )(using
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): Resource[IO, (Context, Classpath)] =
    Resource.eval(resolveClasspath(cwd, module, noCache, config, testScope)).flatMap { paths =>
      ContextResource.make(paths, jreClasspath)
    }

  private def resolveClasspath(
      cwd: Path,
      module: Option[String],
      noCache: Boolean,
      config: Config,
      testScope: Boolean
  )(using tracer: Tracer[IO], logger: Logger[IO]): IO[List[Path]] =
    BuildToolDetector.detectKind(cwd).flatMap { kind =>
      val buildTool = instantiate(kind, cwd, config)
      val useCache  = kind != BuildToolKind.ScalaCli && !noCache
      logger.debug(s"detected build tool: $kind (cache ${if useCache then "enabled" else "disabled"})") >>
        buildTool.validateTestScope(testScope) >> tracer
        .spanBuilder("build.classpath")
        .addAttribute(Attribute("build.tool", kind.toString))
        .build
        .surround {
          if useCache then cachedFlow(buildTool, module, cwd, testScope)
          else buildTool.extractClasspath(module, testScope)
        }
    }

  private def cachedFlow(buildTool: BuildTool, module: Option[String], cwd: Path, testScope: Boolean)(using
      logger: Logger[IO]
  ): IO[List[Path]] =
    val cache = ClasspathCache(cwd)
    val moduleKey = s"${module.getOrElse("")}${if testScope then "/test" else ""}"

    for
      fingerFiles <- buildTool.fingerprintFiles
      hash        <- BuildFingerprint.compute(fingerFiles, moduleKey)
      cached      <- cache.get(hash)
      _           <- logger.debug(s"classpath cache ${if cached.isDefined then "hit" else "miss"} for '$moduleKey'")
      paths <- cached match
        case Some(paths) => buildTool.compile(module, testScope).as(paths)
        case None        => buildTool.extractClasspath(module, testScope).flatTap(paths => cache.put(hash, paths))
    yield paths

  private def instantiate(kind: BuildToolKind, cwd: Path, config: Config): BuildTool = kind match
    case BuildToolKind.Mill     => MillBuildTool(cwd, config.mill)
    case BuildToolKind.Sbt      => SbtBuildTool(cwd, config.sbt)
    case BuildToolKind.ScalaCli => ScalaCliBuildTool(cwd)
