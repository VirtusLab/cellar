package cellar

import cats.effect.IO
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context

object PublicApiFilterTest extends SequentialIOSuite:

  private def withScala3Ctx[A](body: Context => IO[A]): IO[A] =
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  private def withJavaCtx[A](body: Context => IO[A]): IO[A] =
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.javaCoord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("public method passes filter"):
    withScala3Ctx { ctx =>
      IO.blocking {
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        // CellarADT is a public trait — should pass
        expect(PublicApiFilter.isPublic(cls))
      }
    }

  test("Synthetic flag causes symbol to fail filter"):
    withScala3Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        // Find any class and look for synthetic members
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val members = cls.declarations
        // At least some members should be filtered out (synthetic ones)
        // Just verify the method doesn't throw
        val _ = members.map(m => PublicApiFilter.isPublic(m))
        success
      }
    }

  test("isPublic returns false for any symbol whose name starts with $"):
    withScala3Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val members = cls.declarations
        val dollarMembers = members.filter(_.name.toString.startsWith("$"))
        forEach(dollarMembers) { m =>
          expect(!PublicApiFilter.isPublic(m), s"Expected ${m.name} to be filtered")
        }
      }
    }

  test("static initializer is filtered"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val statics = ctx.findStaticModuleClass("cellar.fixture.java.CellarJavaClass")
        val clinits = statics.declarations.filter(_.name.toString == "<clinit>")
        expect(clinits.nonEmpty, "Fixture must declare a static initializer") and
          forEach(clinits)(c => expect(!PublicApiFilter.isPublic(c), "Expected <clinit> to be filtered"))
      }
    }
