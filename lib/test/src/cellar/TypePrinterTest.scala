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

  test("printSymbolSignature renders higher-kinded type params (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val wrap = cls.declarations.find(_.name.toString == "wrap").get
        val sig  = TypePrinter.printSymbolSignature(wrap)
        assertEquals(sig, "def wrap[F[_], A](fa: F[A]): F[A]")
      }
    }

  test("printSymbolSignature renders higher-kinded class type params (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val box        = ctx.findStaticClass("cellar.fixture.scala3.CellarBox")
        val boundedBox = ctx.findStaticClass("cellar.fixture.scala3.CellarBoundedBox")
        assertEquals(TypePrinter.printSymbolSignature(box), "trait CellarBox[F[_]]")
        assertEquals(TypePrinter.printSymbolSignature(boundedBox), "trait CellarBoundedBox[F[_ <: AnyRef]]")
      }
    }

  test("printSymbolSignature renders a standalone type lambda as a type argument (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val compose = cls.declarations.find(_.name.toString == "compose").get
        val sig     = TypePrinter.printSymbolSignature(compose)
        assertEquals(
          sig,
          "def compose[F[_], G[_]](bf: CellarBox[F], bg: CellarBox[G]): CellarBox[[A] =>> F[G[A]]]"
        )
      }
    }

  // TASTy stores a type constructor passed to `F[_]` as `[A] =>> C[A]`. Printing the expansion
  // makes the Scala 3 rendering worse than the Scala 2 one for the same class, so contract it.
  test("printSymbolSignature contracts an eta-expanded type constructor (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala3.CellarSelfBox")
        val sig = TypePrinter.printSymbolSignature(cls)
        assert(!sig.contains("=>>"), s"eta-expansion not contracted: $sig")
        assertEquals(sig, "class CellarSelfBox[A] extends CellarBox[CellarSelfBox]")
      }
    }

  // The mirror of the test above: an author who writes the lambda out is making a different
  // declaration from `type HandWrittenEta = List` (its parameter is invariant), so contracting
  // here would print two distinct sources identically. Contraction is argument-position only.
  test("printSymbolSignature keeps a hand-written eta-expansion in an alias"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val alias = cls.declarations.find(_.name.toString == "HandWrittenEta").get
        assertEquals(TypePrinter.printSymbolSignature(alias), "type HandWrittenEta = [A] =>> List[A]")
      }
    }

  test("printSymbolSignature renders a bounded standalone type lambda as a type argument (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls           = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val composeBounded = cls.declarations.find(_.name.toString == "composeBounded").get
        val sig            = TypePrinter.printSymbolSignature(composeBounded)
        assertEquals(
          sig,
          "def composeBounded[F[_]](bf: CellarBox[F]): CellarBoundedBox[[A <: AnyRef] =>> F[A]]"
        )
      }
    }

  test("printSymbolSignature renders a bounded higher-kinded type param (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val bounded = cls.declarations.find(_.name.toString == "bounded").get
        val sig     = TypePrinter.printSymbolSignature(bounded)
        assertEquals(sig, "def bounded[F[_ <: AnyRef]](fa: F[String]): F[String]")
      }
    }

  test("printSymbolSignature renders a constructor-bounded higher-kinded type param (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val upper = cls.declarations.find(_.name.toString == "upper").get
        val sig   = TypePrinter.printSymbolSignature(upper)
        assertEquals(sig, "def upper[F[A] <: Iterable[A]](fa: F[Int]): F[Int]")
      }
    }

  test("printSymbolSignature renders a self-referential higher-kinded param bound (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls         = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val selfBounded = cls.declarations.find(_.name.toString == "selfBounded").get
        val sig         = TypePrinter.printSymbolSignature(selfBounded)
        assertEquals(sig, "def selfBounded[F[A <: Comparable[A]]](fa: F[String]): F[String]")
      }
    }

  test("printSymbolSignature renders a multi-arity higher-kinded type param (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val bimap = cls.declarations.find(_.name.toString == "bimap").get
        val sig   = TypePrinter.printSymbolSignature(bimap)
        assertEquals(sig, "def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]")
      }
    }

  test("printSymbolSignature renders higher-kinded type params (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val wrap = cls.declarations.find(_.name.toString == "wrap").get
        val sig  = TypePrinter.printSymbolSignature(wrap)
        assertEquals(sig, "def wrap[F[_], A](fa: F[A]): F[A]")
      }
    }

  test("printSymbolSignature renders a bounded higher-kinded type param (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val bounded = cls.declarations.find(_.name.toString == "bounded").get
        val sig     = TypePrinter.printSymbolSignature(bounded)
        assertEquals(sig, "def bounded[F[_ <: AnyRef]](fa: F[String]): F[String]")
      }
    }

  test("printSymbolSignature renders a multi-arity higher-kinded type param (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val bimap = cls.declarations.find(_.name.toString == "bimap").get
        val sig   = TypePrinter.printSymbolSignature(bimap)
        assertEquals(sig, "def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]")
      }
    }

  private def withJavaCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.javaCoord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  // A Java type parameter carries an implicit Nothing lower bound. Printing Nothing as
  // "NothingType" both leaked the internal name and defeated the elision in printTypeParam,
  // yielding `[E >: NothingType <: Comparable[E]]`.
  test("printSymbolSignature elides the implicit Nothing lower bound on a Java type param"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.java.CellarJavaClass")
        val sig = TypePrinter.printSymbolSignature(cls)
        assert(!sig.contains("NothingType"), s"leaked internal type name: $sig")
        assert(sig.startsWith("class CellarJavaClass[T <: Comparable[T]]"), s"unexpected: $sig")
      }
    }

  // Java's Object reaches tasty-query as a TypeRef named `<FromJavaObject>`, which used to print
  // verbatim in every Java signature that mentions Object.
  test("printSymbolSignature renders Java's Object as Object"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("java.lang.String", "equals")
        assert(!sig.contains("FromJavaObject"), s"leaked internal type name: $sig")
        assertEquals(sig, "def equals(x$0: Object): Boolean")
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

  test("printSymbolSignature renders tuple types as paren sugar (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "pair"), "def pair[A, B](t: (A, B)): (A, B)")
        assertEquals(sugarSig(fqn, "triple"), "def triple[A, B, C](t: (A, B, C)): (A, B, C)")
        assertEquals(sugarSig(fqn, "tupleArg"), "def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean")
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

  test("printSymbolSignature renders symbolic binary types as infix (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "mapK")
        assertEquals(sig, "def mapK[F[_], G[_]](f: F ~> G): Unit")
      }
    }

  test("printSymbolSignature collapses trivial wildcard bounds (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "wildcard")
        assertEquals(sig, "def wildcard: List[?]")
      }
    }

  test("printSymbolSignature keeps a real wildcard upper bound (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "boundedWildcard")
        assertEquals(sig, "def boundedWildcard: List[? <: AnyRef]")
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
        val fqn = "cellar.fixture.scala2.CellarSugar"
        assertEquals(sugarSig(fqn, "transform"), "def transform[A, B](f: A => B): B")
        assertEquals(sugarSig(fqn, "zip"), "def zip[A, B, C](f: (A, B) => C): C")
        assertEquals(sugarSig(fqn, "nested"), "def nested[A, B, C](f: (A => B) => C): C")
        assertEquals(sugarSig(fqn, "thunk"), "def thunk[A](f: () => A): A")
      }
    }

  test("printSymbolSignature renders tuple types as paren sugar (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        assertEquals(sugarSig(fqn, "pair"), "def pair[A, B](t: (A, B)): (A, B)")
        assertEquals(sugarSig(fqn, "triple"), "def triple[A, B, C](t: (A, B, C)): (A, B, C)")
        assertEquals(sugarSig(fqn, "tupleArg"), "def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean")
      }
    }

  test("printSymbolSignature collapses trivial wildcard bounds (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala2.CellarSugar", "wildcard")
        assertEquals(sig, "def wildcard: List[?]")
      }
    }

  test("printSymbolSignature keeps a real wildcard upper bound (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala2.CellarSugar", "boundedWildcard")
        assertEquals(sig, "def boundedWildcard: List[? <: AnyRef]")
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
