package cellar

import cats.effect.IO
import munit.CatsEffectSuite

class CoursierFetchClientTest extends CatsEffectSuite:

  test("fetchClasspath with scala3 fixture returns non-empty paths"):
    TestFixtures.assumeFixturesAvailable()
    CoursierFetchClient.fetchClasspath(
      TestFixtures.scala3Coord,
      Seq(TestFixtures.localM2Repo)
    ).map { paths =>
      assert(paths.nonEmpty, "Expected at least one JAR")
      paths.foreach { p =>
        assert(java.nio.file.Files.exists(p.toNioPath), s"Path does not exist: $p")
        assert(p.toString.endsWith(".jar"), s"Expected .jar, got: $p")
      }
    }

  test("fetchClasspath with java fixture returns existing JAR paths"):
    TestFixtures.assumeFixturesAvailable()
    CoursierFetchClient.fetchClasspath(
      TestFixtures.javaCoord,
      Seq(TestFixtures.localM2Repo)
    ).map { paths =>
      assert(paths.nonEmpty)
      assert(paths.forall(p => java.nio.file.Files.exists(p.toNioPath)))
    }

  test("fetchClasspath with non-existent version raises CoordinateNotFound with suggestions"):
    val bad = MavenCoordinate("org.typelevel", "cats-core_3", "99.99.99")
    CoursierFetchClient.fetchClasspath(bad).attempt.map {
      case Left(e: CellarError.CoordinateNotFound) =>
        assertEquals(e.coord, bad)
        assert(e.getCause != null)
        assert(e.suggestions.nonEmpty, "Expected suggestions for existing artifact with wrong version")
      case Left(e)  => fail(s"Expected CoordinateNotFound, got ${e.getClass}: ${e.getMessage}")
      case Right(_) => fail("Expected failure for non-existent version")
    }

  test("fetchClasspath with non-existent group raises CoordinateNotFound"):
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    CoursierFetchClient.fetchClasspath(bad).attempt.map {
      case Left(_: CellarError.CoordinateNotFound) => ()
      case Left(e)                                  => fail(s"Unexpected: ${e.getMessage}")
      case Right(_)                                 => fail("Expected failure")
    }

  test("CoordinateNotFound wraps a non-null cause"):
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    CoursierFetchClient.fetchClasspath(bad).attempt.map {
      case Left(e: CellarError.CoordinateNotFound) => assert(e.getCause != null)
      case _                                        => ()
    }
