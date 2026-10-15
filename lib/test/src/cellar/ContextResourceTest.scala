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
        .use { (ctx, _, sourceJars) =>
          IO.blocking(ctx.findStaticClass("cellar.fixture.scala3.CellarA")).map { cls =>
            assertEquals(cls.name.toString, "CellarA")
            val sources = sourceJars.forSymbol(cls)
            assert(sources.exists(_.fileName.toString.endsWith("-sources.jar")), s"sources jar for CellarA: $sources")
            val option = ctx.findStaticClass("scala.Option")
            assert(sourceJars.forSymbol(option).exists(_.fileName.toString.startsWith("scala-library")), "transitive dependency sources")
          }
        }
    }

  test("makeFromCoord pairs a Java static member with its class's sources jar"):
    TestFixtures.assumeFixturesAvailable()
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource
        .makeFromCoord(TestFixtures.javaCoord, jrePaths, Seq(TestFixtures.localM2Repo))
        .use { (ctx, _, sourceJars) =>
          IO.blocking {
            given tastyquery.Contexts.Context = ctx
            // Java statics live on tasty-query's synthetic `CellarJavaClass$`, which has no classfile.
            val statics = ctx.findStaticModuleClass("cellar.fixture.java.CellarJavaClass")
            val helper  = statics.declarations.find(_.name.toString == "staticHelper").get
            assert(sourceJars.forSymbol(helper).exists(_.fileName.toString.endsWith("-sources.jar")), "static member")
          }
        }
    }

  test("makeWithSources resolves a jar's sources only when a symbol needs them, once"):
    TestFixtures.assumeFixturesAvailable()
    var asked = List.empty[fs2.io.file.Path]
    for
      jrePaths <- JreClasspath.jrtPath()
      resolved <- CoursierFetchClient.fetchClasspathWithSources(TestFixtures.javaCoord, Seq(TestFixtures.localM2Repo))
      unpaired  = resolved.copy(sourcesJars = Map.empty)
      result   <- ContextResource
                    .makeWithSources(unpaired, jrePaths, resolveSources = jar => { asked = jar :: asked; resolved.sourcesJars.get(jar) })
                    .use { (ctx, _, sourceJars) =>
                      IO.blocking {
                        assertEquals(asked, Nil)
                        val cls = ctx.findStaticClass("cellar.fixture.java.CellarJavaClass")
                        assert(sourceJars.forSymbol(cls).exists(_.fileName.toString.endsWith("-sources.jar")))
                        assert(sourceJars.forSymbol(cls).isDefined)
                        assertEquals(asked.map(_.fileName.toString), List("cellar-fixture-java-0.1.0-SNAPSHOT.jar"))
                      }
                    }
    yield result

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
