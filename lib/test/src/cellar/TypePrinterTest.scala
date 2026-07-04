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

  private def withScala2Ctx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala2Coord, Seq(TestFixtures.localM2Repo))
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

  test("printSymbolSignature renders unbounded type params and curried lists (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarCurried")
        val combine = cls.declarations.find(_.name.toString == "combine").get
        val sig     = TypePrinter.printSymbolSignature(combine)
        assertEquals(sig, "def combine[A, B](a: A)(b: B): B")
      }
    }

  test("printSymbolSignature renders unbounded type params and curried lists (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala2.CellarCurried")
        val combine = cls.declarations.find(_.name.toString == "combine").get
        val sig     = TypePrinter.printSymbolSignature(combine)
        assertEquals(sig, "def combine[A, B](a: A)(b: B): B")
      }
    }

  private def sugarSig(fqn: String, method: String)(using ctx: Context): String =
    val cls = ctx.findStaticClass(fqn)
    TypePrinter.printSymbolSignature(cls.declarations.find(_.name.toString == method).get)

  test("printSymbolSignature renders function types as arrow sugar (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "transform"), "def transform[A, B](f: A => B): B")
        assertEquals(sugarSig(fqn, "zip"), "def zip[A, B, C](f: (A, B) => C): C")
        assertEquals(sugarSig(fqn, "nested"), "def nested[A, B, C](f: (A => B) => C): C")
        assertEquals(sugarSig(fqn, "thunk"), "def thunk[A](f: () => A): A")
        assertEquals(sugarSig(fqn, "ctx"), "def ctx[A, B](f: A ?=> B): B")
      }
    }

  test("printSymbolSignature parenthesises a function-typed parent in extends position"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("scala.PartialFunction")
        val sig = TypePrinter.printSymbolSignature(cls)
        assert(sig.contains("extends (A => B)"), s"Expected parenthesised function parent in: $sig")
      }
    }

  test("printSymbolSignature renders implicit and using param lists (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "withImplicit"), "def withImplicit[A](a: A)(implicit s: CellarShow[A]): A")
        assertEquals(sugarSig(fqn, "withUsing"), "def withUsing[A](a: A)(using s: CellarShow[A]): A")
      }
    }

  test("printSymbolSignature renders function types as arrow sugar (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        assertEquals(
          sugarSig("cellar.fixture.scala2.CellarSugar", "transform"),
          "def transform[A, B](f: A => B): B"
        )
      }
    }

  test("printSymbolSignature renders an implicit param list (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        assertEquals(
          sugarSig("cellar.fixture.scala2.CellarSugar", "withImplicit"),
          "def withImplicit[A](a: A)(implicit s: CellarShow[A]): A"
        )
      }
    }
