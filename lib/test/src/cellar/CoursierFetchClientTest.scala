package cellar

import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import weaver.SimpleIOSuite

object CoursierFetchClientTest extends SimpleIOSuite:

  test("fetchClasspath with scala3 fixture returns non-empty paths"):
    TestFixtures.assumeFixturesAvailable *>
      CoursierFetchClient.fetchClasspath(
        TestFixtures.scala3Coord,
        Seq(TestFixtures.localM2Repo)
      ).map { paths =>
        expect(paths.nonEmpty, "Expected at least one JAR") and
          forEach(paths.toList) { p =>
            expect(java.nio.file.Files.exists(p.toNioPath), s"Path does not exist: $p") and
              expect(p.toString.endsWith(".jar"), s"Expected .jar, got: $p")
          }
      }

  test("fetchClasspath with java fixture returns existing JAR paths"):
    TestFixtures.assumeFixturesAvailable *>
      CoursierFetchClient.fetchClasspath(
        TestFixtures.javaCoord,
        Seq(TestFixtures.localM2Repo)
      ).map { paths =>
        expect.all(
          paths.nonEmpty,
          paths.forall(p => java.nio.file.Files.exists(p.toNioPath))
        )
      }

  test("fetchClasspath with non-existent version raises CoordinateNotFound with suggestions"):
    val bad = MavenCoordinate("org.typelevel", "cats-core_3", "99.99.99")
    CoursierFetchClient.fetchClasspath(bad).attempt.map {
      case Left(e: CellarError.CoordinateNotFound) =>
        expect(clue(e.coord) == bad) and
          expect(e.getCause != null) and
          expect(e.suggestions.nonEmpty, "Expected suggestions for existing artifact with wrong version")
      case Left(e)  => failure(s"Expected CoordinateNotFound, got ${e.getClass}: ${e.getMessage}")
      case Right(_) => failure("Expected failure for non-existent version")
    }

  test("fetchClasspath with non-existent group raises CoordinateNotFound"):
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    CoursierFetchClient.fetchClasspath(bad).attempt.map {
      case Left(_: CellarError.CoordinateNotFound) => success
      case Left(e)                                  => failure(s"Unexpected: ${e.getMessage}")
      case Right(_)                                 => failure("Expected failure")
    }

  test("CoordinateNotFound wraps a non-null cause"):
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    CoursierFetchClient.fetchClasspath(bad).attempt.map {
      case Left(e: CellarError.CoordinateNotFound) => expect(e.getCause != null)
      case _                                        => success
    }
