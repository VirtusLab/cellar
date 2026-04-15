package cellar

import fs2.io.file.Path
import pureconfig.*

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(binary: String, extraArgs: String) derives ConfigReader {
  def effectiveExtraArgs: List[String] = extraArgs.split("\\s+").filter(_.nonEmpty).toList
}

case class StarvationChecksConfig(enabled: Boolean) derives ConfigReader

case class Config(mill: MillConfig, sbt: SbtConfig, starvationChecks: StarvationChecksConfig) derives ConfigReader

object Config {
  private val defaultUserPath: Option[Path] =
    sys.props.get("user.home").map(Path(_).resolve(".cellar").resolve("cellar.conf"))
  private val defaultProjectPath: Path = Path(".cellar").resolve("cellar.conf")

  lazy val global: Config = {
    val paths = (defaultUserPath.toList ++ List(defaultProjectPath))
      .filter(p => java.nio.file.Files.exists(p.toNioPath))
    paths
      .foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p.toNioPath).withFallback(cs))
      .loadOrThrow[Config]
  }
}
