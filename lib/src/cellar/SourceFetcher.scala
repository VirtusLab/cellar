package cellar

import cats.effect.IO
import coursierapi.Repository
import fs2.io.file.Path

import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*

object SourceFetcher:
  case class SourceResult(entryPath: String, startLine: Int, endLine: Int, lines: IndexedSeq[String])

  def fetch(
      coord: MavenCoordinate,
      sourceFilePath: String,
      startLine: Int,
      endLine: Int,
      extraRepositories: Seq[Repository] = Seq.empty
  ): IO[Either[String, SourceResult]] =
    CoursierFetchClient.fetchSourcesJar(coord, extraRepositories).flatMap {
      case None =>
        IO.pure(Left(s"No sources JAR published for '${coord.render}'."))
      case Some(jar) =>
        IO.blocking(extractLines(jar, sourceFilePath, startLine, endLine))
    }

  private def extractLines(
      jar: Path,
      sourceFilePath: String,
      startLine: Int,
      endLine: Int
  ): Either[String, SourceResult] =
    val normalizedSource = sourceFilePath.replace('\\', '/')
    val zip = ZipFile(jar.toNioPath.toFile)
    try
      val entry = zip.entries().asScala.find { e =>
        !e.isDirectory && normalizedSource.endsWith(e.getName)
      }
      entry match
        case None =>
          Left(s"Source file not found in JAR (looked for suffix of '$normalizedSource').")
        case Some(e) =>
          val allLines = scala.io.Source.fromInputStream(zip.getInputStream(e), "UTF-8")
            .getLines().toIndexedSeq
          val extracted = allLines.slice(startLine, endLine + 1)
          Right(SourceResult(e.getName, startLine, endLine, extracted))
    finally
      zip.close()
