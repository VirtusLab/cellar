package cellar

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.{Files, Path}
import munit.CatsEffectSuite

class JavaConfigTest extends CatsEffectSuite:

  private def loadWith(userConfig: Option[String], projectConfig: Option[String]): IO[Config] =
    Files[IO].tempDirectory.use { dir =>
      def write(name: String, contents: String): IO[Path] =
        val file = dir.resolve(name)
        fs2.Stream.emit(contents).through(Files[IO].writeUtf8(file)).compile.drain.as(file)

      for
        userPath    <- userConfig.traverse(write("user.conf", _))
        projectPath <- projectConfig.traverse(write("project.conf", _))
      yield Config.loadFrom(userPath, projectPath)
    }

  test("splits at the first '=' and trims key and value"):
    // given
    val config = JavaConfig(List("javax.net.ssl.trustStore = /tmp/a=b/cacerts", "empty.value="))

    // when
    val parsed = config.parsed

    // then
    assertEquals(parsed, Right(List("javax.net.ssl.trustStore" -> "/tmp/a=b/cacerts", "empty.value" -> "")))

  test("rejects entries without '=' or with an empty key"):
    List("javax.net.ssl.trustStore", "=value", " = value", "").foreach { entry =>
      // when
      val parsed = JavaConfig(List(entry)).parsed

      // then
      assert(parsed.isLeft, s"Expected '$entry' to be rejected, got $parsed")
    }

  test("default configuration sets no properties"):
    // when / then
    loadWith(None, None).map(config => assertEquals(config.java.properties, Nil))

  test("project-level properties are ignored, user-level ones apply"):
    // given
    val user    = """java.properties = ["javax.net.ssl.trustStore=/user/cacerts"]"""
    val project = """java.properties = ["javax.net.ssl.trustStore=/project/cacerts"]"""

    // when / then
    loadWith(Some(user), Some(project)).map { config =>
      assertEquals(config.java.properties, List("javax.net.ssl.trustStore=/user/cacerts"))
    }
