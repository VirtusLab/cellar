package cellar

import cats.effect.{IO, Resource}
import fs2.io.file.{Files, Path}

import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.util.Using
import weaver.SimpleIOSuite

object SourceFetcherTest extends SimpleIOSuite:

  private def jarWith(files: (String, String)*): Resource[IO, Path] =
    Files[IO].tempFile(None, "sources-", ".jar", None).evalTap { jar =>
      IO.blocking {
        Using.resource(ZipOutputStream(java.nio.file.Files.newOutputStream(jar.toNioPath))) { zip =>
          files.foreach { (name, text) =>
            zip.putNextEntry(ZipEntry(name))
            zip.write(text.getBytes("UTF-8"))
            zip.closeEntry()
          }
        }
      }
    }

  private def fetched(jar: Path, path: String): IO[Either[String, String]] =
    SourceFetcher.fetch(jar, path, 0, Int.MaxValue).map(_.map(r => s"${r.entryPath}:${r.lines.mkString}"))

  test("an entry is not taken for a file whose name merely ends with it"):
    jarWith("Path.scala" -> "path", "ResourcePath.scala" -> "resource").use { jar =>
      fetched(jar, "ResourcePath.scala").map(r => expect(clue(r) == Right("ResourcePath.scala:resource")))
    }

  test("the most specific entry wins for a path with a directory prefix"):
    jarWith("Foo.scala" -> "root", "sub/Foo.scala" -> "sub").use { jar =>
      for
        exact <- fetched(jar, "sub/Foo.scala")
        build <- fetched(jar, "/build/src/main/scala/sub/Foo.scala")
        root  <- fetched(jar, "Foo.scala")
      yield
        expect(clue(exact) == Right("sub/Foo.scala:sub")) and
          expect(clue(build) == Right("sub/Foo.scala:sub")) and
          expect(clue(root) == Right("Foo.scala:root"))
    }
