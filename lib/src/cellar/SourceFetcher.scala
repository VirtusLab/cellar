package cellar

import cats.effect.{IO, Resource}
import coursierapi.Repository
import fs2.io.file.Path
import fs2.io.readInputStream

import java.util.zip.{ZipEntry, ZipFile}
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
        val entries = zip.entries().asScala.filter(!_.isDirectory).toVector
        entries
          .find(e => normalizedSource.endsWith(e.getName))
          .orElse(findDeclaringFile(zip, entries, normalizedSource))
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

  /**
   * Scala 2 sources frequently live in a file that isn't named after the type
   * (`package.scala`, several types per file), so when the guessed path is missing,
   * look for a sibling `.scala` file in the same directory that declares the name.
   */
  private def findDeclaringFile(zip: ZipFile, entries: Vector[ZipEntry], guessedPath: String): Option[ZipEntry] =
    val slash = guessedPath.lastIndexOf('/')
    val dir = guessedPath.substring(0, slash + 1)
    val name = guessedPath.substring(slash + 1).stripSuffix(".scala")
    if !guessedPath.endsWith(".scala") then None
    else
      val declaration = raw"""(?m)^\s*(?:\w+\s+)*(?:class|trait|object|type)\s+`?${java.util.regex.Pattern.quote(name)}`?(?![\w$$])""".r
      entries
        .filter(e => e.getName.endsWith(".scala") && e.getName.startsWith(dir) && !e.getName.drop(dir.length).contains('/'))
        .find { e =>
          val text = String(zip.getInputStream(e).readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
          declaration.findFirstIn(text).isDefined
        }
