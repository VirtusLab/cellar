package cellar

import fs2.io.file.Path
import pureconfig.*

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(binary: String, extraArgs: String) derives ConfigReader {
  def effectiveExtraArgs: List[String] = extraArgs.split("\\s+").filter(_.nonEmpty).toList
}

case class StarvationChecksConfig(enabled: Boolean) derives ConfigReader

case class ProfilingConfig(enabled: Boolean, pyroscopeEndpoint: String) derives ConfigReader

case class OtelConfig(enabled: Boolean, endpoint: String) derives ConfigReader

case class Config(
    mill: MillConfig,
    sbt: SbtConfig,
    starvationChecks: StarvationChecksConfig,
    profiling: ProfilingConfig,
    otel: OtelConfig
) derives ConfigReader

object Config {
  private val defaultUserPath: Option[Path] =
    sys.props.get("user.home").map(Path(_).resolve(".cellar").resolve("cellar.conf"))
  private[cellar] val defaultProjectPath: Path = Path(".cellar").resolve("cellar.conf")

  private def load(): Config = {
    val paths = (defaultUserPath.toList ++ List(defaultProjectPath))
      .filter(p => java.nio.file.Files.exists(p.toNioPath))
    paths
      .foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p.toNioPath).withFallback(cs))
      .loadOrThrow[Config]
  }

  lazy val global: Config = load()

  def loadFresh(): Config = load()
}
