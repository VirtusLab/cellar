package cellar.build

import cats.effect.{IO, Resource}
import cellar.{Config, ContextResource}

import java.nio.file.Path
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object ProjectClasspathProvider:
  def provide(
      cwd: Path,
      module: Option[String],
      jreClasspath: Classpath,
      noCache: Boolean,
      config: Config
  ): Resource[IO, (Context, Classpath)] =
    Resource.eval(resolveClasspath(cwd, module, noCache, config)).flatMap { paths =>
      ContextResource.make(paths, jreClasspath)
    }

  private def resolveClasspath(cwd: Path, module: Option[String], noCache: Boolean, config: Config): IO[List[Path]] =
    BuildToolDetector.detectKind(cwd).flatMap { kind =>
      val buildTool = instantiate(kind, cwd, config)
      val useCache = kind != BuildToolKind.ScalaCli && !noCache

      if useCache then cachedFlow(buildTool, module, cwd)
      else buildTool.extractClasspath(module)
    }

  private def cachedFlow(buildTool: BuildTool, module: Option[String], cwd: Path): IO[List[Path]] =
    val cache = ClasspathCache(cwd)
    val moduleKey = module.getOrElse("")

    for
      fingerFiles <- buildTool.fingerprintFiles()
      hash        <- BuildFingerprint.compute(fingerFiles, moduleKey)
      cached      <- cache.get(hash)
      paths <- cached match
        case Some(paths) => buildTool.compile(module).as(paths)
        case None        => buildTool.extractClasspath(module).flatTap(paths => cache.put(hash, paths))
    yield paths

  private def instantiate(kind: BuildToolKind, cwd: Path, config: Config): BuildTool = kind match
    case BuildToolKind.Mill     => MillBuildTool(cwd, config.mill)
    case BuildToolKind.Sbt      => SbtBuildTool(cwd, config.sbt)
    case BuildToolKind.ScalaCli => ScalaCliBuildTool(cwd)
