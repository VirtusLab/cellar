package cellar

import cats.effect.IO
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

object ContextResourceTest extends SequentialIOSuite:

  test("make with valid jrePaths and no jars succeeds"):
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource.make(Seq.empty, jrePaths).use { (ctx, _) =>
        IO.blocking(ctx.findStaticClass("java.lang.String")).map { cls =>
          expect(cls.name.toString == "String")
        }
      }
    }

  test("make with scala3 fixture finds a known class"):
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord,
                    Seq(TestFixtures.localM2Repo)
                  )
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) =>
                    IO.blocking {
                      ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
                    }.map(cls => expect(cls.name.toString == "CellarADT"))
                  }
    yield result

  test("makeFromCoord with scala3 fixture succeeds"):
    TestFixtures.assumeFixturesAvailable *>
      JreClasspath.jrtPath().flatMap { jrePaths =>
        ContextResource
          .makeFromCoord(TestFixtures.scala3Coord, jrePaths, Seq(TestFixtures.localM2Repo))
          .use { (ctx, _, sourceJars) =>
            IO.blocking(ctx.findStaticClass("cellar.fixture.scala3.CellarA")).map { cls =>
              val sources = sourceJars.forSymbol(cls)
              val option  = ctx.findStaticClass("scala.Option")
              expect.eql("CellarA", cls.name.toString) and
                expect(sources.exists(_.fileName.toString.endsWith("-sources.jar")), s"sources jar for CellarA: $sources") and
                expect(sourceJars.forSymbol(option).exists(_.fileName.toString.startsWith("scala-library")), "transitive dependency sources")
            }
          }
      }

  test("makeFromCoord pairs a Java static member with its class's sources jar"):
    TestFixtures.assumeFixturesAvailable *>
      JreClasspath.jrtPath().flatMap { jrePaths =>
        ContextResource
          .makeFromCoord(TestFixtures.javaCoord, jrePaths, Seq(TestFixtures.localM2Repo))
          .use { (ctx, _, sourceJars) =>
            IO.blocking {
              given tastyquery.Contexts.Context = ctx
              // Java statics live on tasty-query's synthetic `CellarJavaClass$`, which has no classfile.
              val statics = ctx.findStaticModuleClass("cellar.fixture.java.CellarJavaClass")
              val helper  = statics.declarations.find(_.name.toString == "staticHelper").get
              expect(sourceJars.forSymbol(helper).exists(_.fileName.toString.endsWith("-sources.jar")), "static member")
            }
          }
      }

  test("makeWithSources resolves a jar's sources only when a symbol needs them, once"):
    var asked = List.empty[fs2.io.file.Path]
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      resolved <- CoursierFetchClient.fetchClasspathWithSources(TestFixtures.javaCoord, Seq(TestFixtures.localM2Repo))
      unpaired  = resolved.copy(sourcesJars = Map.empty)
      result   <- ContextResource
                    .makeWithSources(unpaired, jrePaths, resolveSources = jar => { asked = jar :: asked; resolved.sourcesJars.get(jar) })
                    .use { (ctx, _, sourceJars) =>
                      IO.blocking {
                        val askedBefore = asked
                        val cls         = ctx.findStaticClass("cellar.fixture.java.CellarJavaClass")
                        expect(askedBefore.isEmpty) and
                          expect(sourceJars.forSymbol(cls).exists(_.fileName.toString.endsWith("-sources.jar"))) and
                          expect(sourceJars.forSymbol(cls).isDefined) and
                          expect.eql(List("cellar-fixture-java-0.1.0-SNAPSHOT.jar"), asked.map(_.fileName.toString))
                      }
                    }
    yield result

  test("makeFromCoord with bad coordinate raises CoordinateNotFound"):
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource.makeFromCoord(bad, jrePaths).use(_ => IO.unit).attempt.map {
        case Left(_: CellarError.CoordinateNotFound) => success
        case Left(e)                                  => failure(s"Unexpected: ${e.getMessage}")
        case Right(_)                                 => failure("Expected failure")
      }
    }

  test("Resource finalizer runs even if body raises exception"):
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource.make(Seq.empty, jrePaths).use { _ =>
        IO.raiseError(new RuntimeException("intentional"))
      }.attempt.map { result =>
        expect(result.isLeft, "Expected the exception to propagate")
      }
    }
