package cellar.build

import cats.effect.IO
import java.nio.file.Path

trait BuildTool:
  def name: String
  def compile(module: Option[String]): IO[Unit]
  def extractClasspath(module: Option[String]): IO[List[Path]]
  def fingerprintFiles(module: Option[String]): IO[List[Path]]
