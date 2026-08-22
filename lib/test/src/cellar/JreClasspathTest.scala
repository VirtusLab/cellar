package cellar

import cats.effect.IO
import fs2.io.file.{Files, Path}
import munit.CatsEffectSuite

class JreClasspathTest extends CatsEffectSuite:

  // No `assume` on JAVA_HOME: the point is that the zero-arg form works without it, by falling
  // back to the JVM running the test.
  test("zero-arg jrtPath returns non-empty classpath with or without JAVA_HOME"):
    JreClasspath.jrtPath().map { classpath =>
      assert(classpath.nonEmpty)
    }

  test("one-arg jrtPath with current java.home succeeds"):
    val javaHome = Path(System.getProperty("java.home"))
    JreClasspath.jrtPath(javaHome).map { classpath =>
      assert(classpath.nonEmpty)
    }

  test("one-arg jrtPath with non-existent path raises error"):
    val badPath = Path("/tmp/nonexistent-jdk-home-12345")
    JreClasspath.jrtPath(badPath).attempt.map { result =>
      assert(result.isLeft, "Expected an error for non-existent path")
    }

  test("one-arg jrtPath with plain directory (no jrt-fs.jar) raises IllegalArgumentException"):
    Files[IO].tempDirectory.use { tmpDir =>
      JreClasspath.jrtPath(tmpDir).attempt.map { result =>
        assert(result.isLeft)
        result.left.foreach {
          case _: IllegalArgumentException => ()
          case e => fail(s"Expected IllegalArgumentException, got ${e.getClass}")
        }
      }
    }
