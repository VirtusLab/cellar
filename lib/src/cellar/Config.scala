package cellar

import cats.effect.IO
import cats.syntax.all.*
import pureconfig.*

import java.nio.file.{Files, Path}

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(useClientMode: Boolean, binary: String) derives ConfigReader

case class Config(mill: MillConfig, sbt: SbtConfig) derives ConfigReader

object Config {
  val default: IO[Config] = load(None)

  val defaultUserPath: Option[Path] = sys.props.get("user.home").map(Path.of(_).resolve(".cellar").resolve("cellar.conf"))
  val defaultProjectPath: Path = Path.of(".cellar").resolve("cellar.conf")

  def load(path: Option[Path]): IO[Config] = {
    def load0(path: List[Path]) =
      IO.blocking {
        path.foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p).withFallback(cs)).loadOrThrow[Config]
      }

    path match
      case sp: Some[_] => load0(sp.toList)
      case None =>
        val relevantPaths = defaultUserPath.toList ++ List(defaultProjectPath)
        relevantPaths.filterA(p => IO.blocking(Files.exists(p))).flatMap(load0)
  }
}
