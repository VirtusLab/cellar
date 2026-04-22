package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context

class TypePrinterTest extends CatsEffectSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("detectLanguage returns Scala3 for scala3 fixture symbol"):
    withCtx { ctx =>
      IO.blocking {
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val lang = TypePrinter.detectLanguage(cls)
        assertEquals(lang, DetectedLanguage.Scala3)
      }
    }

  test("detectLanguage returns Java for java.lang.String"):
    withCtx { ctx =>
      IO.blocking {
        val cls  = ctx.findStaticClass("java.lang.String")
        val lang = TypePrinter.detectLanguage(cls)
        assertEquals(lang, DetectedLanguage.Java)
      }
    }

  test("printSymbolSignature for trait contains 'trait' keyword"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val sig = TypePrinter.printSymbolSignature(cls)
        assert(sig.contains("trait"), s"Expected 'trait' in: $sig")
      }
    }

  test("printSymbolSignature for case class contains 'class' keyword"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val sig = TypePrinter.printSymbolSignature(cls)
        assert(sig.contains("class"), s"Expected 'class' in: $sig")
      }
    }

  test("printSymbolSignatureSafe for Scala2 symbol appends Scala 2 comment"):
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala2Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) =>
                  IO.blocking {
                    given Context = ctx
                    val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarTypeClass")
                    val sig = TypePrinter.printSymbolSignatureSafe(cls)
                    assert(sig.contains("Scala 2"), s"Expected Scala 2 annotation in: $sig")
                  }
                }
    yield result

  test("printSymbolSignature for companion object term does not double the name"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val term = ctx.findStaticTerm("cellar.fixture.scala3.CellarTC")
        val sig  = TypePrinter.printSymbolSignature(term)
        assertEquals(sig, "object CellarTC")
      }
    }

  test("printSymbolSignature for companion object class prints module class name"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarTC")
        val sig = TypePrinter.printSymbolSignature(cls)
        assert(sig.startsWith("object"), s"Expected 'object' keyword in: $sig")
      }
    }

  test("printSymbolSignatureSafe does not throw for Java symbol"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("java.lang.String")
        val sig = TypePrinter.printSymbolSignatureSafe(cls)
        assert(sig.nonEmpty)
      }
    }
