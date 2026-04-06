package cellar

import cats.effect.IO
import pureconfig.*

import java.nio.file.{Files, Path}

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(useClientMode: Boolean, binary: String) derives ConfigReader

case class Config(mill: MillConfig, sbt: SbtConfig) derives ConfigReader

object Config {
  val default: IO[Config] = load(None)

  val defaultPath: Path = Path.of(".cellar").resolve("cellar.conf")

  def load(path: Option[Path]): IO[Config] = {
    def load0(path: Option[Path]) =
      IO.blocking {
        path.foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p).withFallback(cs)).loadOrThrow[Config]
      }

    path match
      case sp: Some[_] => load0(sp)
      case None => IO.blocking(Files.exists(defaultPath)).flatMap {
        case true  => load0(Some(defaultPath))
        case false => load0(None)
      }
  }
}
