package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import java.nio.file.{Files, Path}

class JreClasspathTest extends CatsEffectSuite:

  test("zero-arg jrtPath with JAVA_HOME set returns non-empty classpath"):
    assume(sys.env.contains("JAVA_HOME"), "JAVA_HOME not set")
    JreClasspath.jrtPath().map { classpath =>
      assert(classpath.nonEmpty)
    }

  test("one-arg jrtPath with current java.home succeeds"):
    val javaHome = Path.of(System.getProperty("java.home"))
    JreClasspath.jrtPath(javaHome).map { classpath =>
      assert(classpath.nonEmpty)
    }

  test("one-arg jrtPath with non-existent path raises error"):
    val badPath = Path.of("/tmp/nonexistent-jdk-home-12345")
    JreClasspath.jrtPath(badPath).attempt.map { result =>
      assert(result.isLeft, "Expected an error for non-existent path")
    }

  test("one-arg jrtPath with plain directory (no jrt-fs.jar) raises IllegalArgumentException"):
    val tmpDir = Files.createTempDirectory("cellar-test")
    JreClasspath.jrtPath(tmpDir).attempt.map { result =>
      assert(result.isLeft)
      result.left.foreach {
        case _: IllegalArgumentException => ()
        case e                           => fail(s"Expected IllegalArgumentException, got ${e.getClass}")
      }
    }.guarantee(IO.blocking(Files.deleteIfExists(tmpDir)).void)
