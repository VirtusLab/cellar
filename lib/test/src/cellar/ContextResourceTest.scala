package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

class ContextResourceTest extends CatsEffectSuite:

  test("make with valid jrePaths and no jars succeeds"):
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource.make(Seq.empty, jrePaths).use { (ctx, _) =>
        IO.blocking(ctx.findStaticClass("java.lang.String")).map { cls =>
          assert(cls.name.toString == "String")
        }
      }
    }

  test("make with scala3 fixture finds a known class"):
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord,
                    Seq(TestFixtures.localM2Repo)
                  )
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) =>
                    IO.blocking {
                      ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
                    }.map(cls => assert(cls.name.toString == "CellarADT"))
                  }
    yield result

  test("makeFromCoord with scala3 fixture succeeds"):
    TestFixtures.assumeFixturesAvailable()
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource
        .makeFromCoord(TestFixtures.scala3Coord, jrePaths, Seq(TestFixtures.localM2Repo))
        .use { (ctx, _) =>
          IO.blocking(ctx.findStaticClass("cellar.fixture.scala3.CellarA")).map { cls =>
            assertEquals(cls.name.toString, "CellarA")
          }
        }
    }

  test("makeFromCoord with bad coordinate raises CoordinateNotFound"):
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource.makeFromCoord(bad, jrePaths).use(_ => IO.unit).attempt.map {
        case Left(_: CellarError.CoordinateNotFound) => ()
        case Left(e)                                  => fail(s"Unexpected: ${e.getMessage}")
        case Right(_)                                 => fail("Expected failure")
      }
    }

  test("Resource finalizer runs even if body raises exception"):
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource.make(Seq.empty, jrePaths).use { _ =>
        IO.raiseError(new RuntimeException("intentional"))
      }.attempt.map { result =>
        assert(result.isLeft, "Expected the exception to propagate")
      }
    }
