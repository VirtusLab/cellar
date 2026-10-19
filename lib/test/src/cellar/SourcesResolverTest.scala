package cellar

import cats.effect.{IO, Resource}
import fs2.io.file.{Files, Path}

import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.util.Using
import weaver.SimpleIOSuite

object SourcesResolverTest extends SimpleIOSuite:

  private def writeJar(jar: Path, files: (String, String)*): IO[Unit] =
    IO.blocking {
      Using.resource(ZipOutputStream(java.nio.file.Files.newOutputStream(jar.toNioPath))) { zip =>
        files.foreach { (name, text) =>
          zip.putNextEntry(ZipEntry(name))
          zip.write(text.getBytes("UTF-8"))
          zip.closeEntry()
        }
      }
    }

  private val tempDir: Resource[IO, Path] = Files[IO].tempDirectory

  test("a sources jar next to the jar is used without any download"):
    tempDir.use { dir =>
      for
        _ <- writeJar(dir / "lib-1.0.jar", "A.class" -> "")
        _ <- writeJar(dir / "lib-1.0-sources.jar", "A.scala" -> "")
      yield expect(clue(SourcesResolver.forJar(dir / "lib-1.0.jar", Nil)) == Some(dir / "lib-1.0-sources.jar"))
    }

  test("a jar naming its own coordinate in pom.properties gets that artifact's sources"):
    val c   = TestFixtures.javaCoord
    val pom = s"groupId=${c.group}\nartifactId=${c.artifact}\nversion=${c.version}\n"
    TestFixtures.assumeFixturesAvailable *>
      tempDir.use { dir =>
        // Outside any cache layout and without a sibling, like a jar copied into a project's lib/.
        val jar = dir / s"${c.artifact}-${c.version}.jar"
        writeJar(jar, s"META-INF/maven/${c.group}/${c.artifact}/pom.properties" -> pom).map { _ =>
          val found = SourcesResolver.forJar(jar, Seq(TestFixtures.localM2Repo))
          expect(found.exists(_.fileName.toString == s"${c.artifact}-${c.version}-sources.jar"), found.toString)
        }
      }

  test("a jar that is no published artifact has no sources"):
    tempDir.use { dir =>
      val jar = dir / "out.jar"
      writeJar(jar, "A.class" -> "").map(_ => expect(clue(SourcesResolver.forJar(jar, Nil)) == None))
    }
