package cellar

import cats.effect.{IO, Resource}
import coursierapi.Repository
import fs2.io.file.Path
import fs2.io.readInputStream

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
        extractLines(jar, sourceFilePath, startLine, endLine)
    }

  private def extractLines(
      jar: Path,
      sourceFilePath: String,
      startLine: Int,
      endLine: Int
  ): IO[Either[String, SourceResult]] =
    val normalizedSource = sourceFilePath.replace('\\', '/')
    Resource.fromAutoCloseable(IO.blocking(ZipFile(jar.toNioPath.toFile))).use { zip =>
      IO.blocking {
        zip.entries().asScala
          .find(e => !e.isDirectory && normalizedSource.endsWith(e.getName))
          .map(e => (e.getName, zip.getInputStream(e)))
      }.flatMap {
        case None =>
          IO.pure(Left(s"Source file not found in JAR (looked for suffix of '$normalizedSource')."))
        case Some((name, is)) =>
          readInputStream(IO.pure(is), chunkSize = 65536)
            .through(fs2.text.utf8.decode)
            .through(fs2.text.lines)
            .compile
            .toVector
            .map { allLines =>
              val extracted = if endLine == Int.MaxValue then allLines.drop(startLine)
                              else allLines.slice(startLine, endLine + 1)
              Right(SourceResult(name, startLine, endLine, extracted))
            }
      }
    }
