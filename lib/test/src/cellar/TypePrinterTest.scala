package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol}

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

  private def withSourcesCtx[A](coord: MavenCoordinate)(body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    JreClasspath.jrtPath().flatMap { jrePaths =>
      ContextResource.makeFromCoord(coord, jrePaths, Seq(TestFixtures.localM2Repo)).use { (ctx, _, _) => body(ctx) }
    }

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

  test("printSymbolSignatureSafe prints a Scala 2 symbol without a language marker"):
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
                    assertEquals(sig, "trait CellarTypeClass[A]")
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
        assertEquals(sig, "def equals(anObject: Object): Boolean")
      }
    }

  test("printSymbolSignature names Java parameters from LocalVariableTable when MethodParameters is absent"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.java.CellarJavaClass"
        val cls = ctx.findStaticClass(fqn)
        val formats = cls.declarations.filter(_.name.toString == "format").map(TypePrinter.printSymbolSignature).sorted
        assertEquals(
          formats,
          List(
            "def format(value: Int): String",
            "def format(value: Int, verbose: Boolean): String",
            "def format(value: String): String"
          )
        )
        assertEquals(sugarSig(fqn, "repeat"), "def repeat(value: T, times: Int): List[T]")
        assertEquals(sugarSig(fqn, "<init>"), "def <init>[T <: Comparable[T]](defaultValue: T): Unit")
        val module = ctx.findStaticModuleClass(fqn)
        val of     = module.declarations.find(_.name.toString == "of").get
        assertEquals(TypePrinter.printSymbolSignature(of), "def of[E <: Comparable[E]](value: E): CellarJavaClass[E]")
      }
    }

  test("printSymbolSignature names Java parameters typed by another class's nested type"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = ctx.findStaticClass("java.util.AbstractMap.SimpleEntry").declarations
          .filter(_.name.toString == "<init>").map(TypePrinter.printSymbolSignature)
        assert(sig.contains("def <init>[K <: Object, V <: Object](entry: Map.Entry[? <: K, ? <: V]): Unit"), sig)
      }
    }

  test("printSymbolSignature names abstract Java parameters from the sources jar"):
    withSourcesCtx(TestFixtures.javaCoord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.java.CellarJavaInterface"
        assertEquals(sugarSig(fqn, "identity"), "def identity(value: T): T")
        assertEquals(sugarSig(fqn, "repeat"), "def repeat(value: T, times: Int): List[T]")
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

  test("printSymbolSignature marks parameters that have a default (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "withDefault"), "def withDefault(a: Int, b: String = ...): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "def curriedDefault(a: Int)(b: Int = ...): Int")
      }
    }

  test("printSymbolSignature prints default values from the sources jar (Scala 3)"):
    withSourcesCtx(TestFixtures.scala3Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "withDefault"), "def withDefault(a: Int, b: String = \"b\"): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "def curriedDefault(a: Int)(b: Int = a): Int")
      }
    }

  test("printSymbolSignature prints default values from the sources jar (Scala 2)"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        assertEquals(sugarSig(fqn, "withDefault"), "def withDefault(a: Int, b: String = \"b\"): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "def curriedDefault(a: Int)(b: Int = a): Int")
      }
    }

  test("printSymbolSignature takes defaults from the source file of the symbol's own package"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        assertEquals(
          sugarSig("cellar.fixture.scala2.CellarSugar", "withDefault"),
          "def withDefault(a: Int, b: String = \"b\"): String"
        )
        assertEquals(
          sugarSig("cellar.fixture.scala2.nested.CellarSugar", "withDefault"),
          "def withDefault(a: Int, b: String = \"nested\"): String"
        )
      }
    }

  private def decls(owner: ClassSymbol, name: String)(using Context): List[Symbol] =
    owner.declarations.filter(_.name.toString == name)

  private def methodSig(owner: ClassSymbol, name: String)(using Context): String =
    TypePrinter.printSymbolSignature(decls(owner, name).head)

  test("sources: overloads with the same parameter names take their own Scaladoc and position"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        val byType = decls(cls, "process").map { sym =>
          TypePrinter.printSymbolSignature(sym) -> (ScalaSources.docFor(sym), ScalaSources.location(sym).map(_.startLine))
        }.toMap
        assertEquals(byType("def process(value: Int): String"), (Some("/** Int process. */"), Some(5)))
        assertEquals(byType("def process(value: String): String"), (Some("/** String process. */"), Some(7)))
      }
    }

  test("sources: a val and a type of the same name keep their own Scaladoc"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        val docs = decls(cls, "Thing").map(s => s.isType -> ScalaSources.docFor(s)).toMap
        assertEquals(docs, Map(false -> Some("/** Value Thing. */"), true -> Some("/** Type Thing. */")))
      }
    }

  test("sources: context-bound evidence does not stop a method from matching its source"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        assert(methodSig(cls, "withBound").contains("b: Int = 3"), methodSig(cls, "withBound"))
      }
    }

  test("sources: context bounds and anonymous using clauses match in Scala 3"):
    withSourcesCtx(TestFixtures.scala3Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala3.CellarScala3Sources")
        assert(methodSig(cls, "withBound").contains("b: Int = 3"), methodSig(cls, "withBound"))
        assert(methodSig(cls, "withUsingDefault").contains("a: Int = 2"), methodSig(cls, "withUsingDefault"))
      }
    }

  test("sources: a multi-line default with a comment is elided rather than flattened"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        assertEquals(methodSig(cls, "commented"), "def commented(xs: List[Int] = ...): Int")
      }
    }

  test("sources: a Scaladoc is found past an ordinary comment"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        assertEquals(ScalaSources.docFor(decls(cls, "afterComment").head), Some("/** Has a doc. */"))
      }
    }

  test("sources: an object nested in a trait is found through its module val"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        val lone = decls(cls, "Lone").collectFirst { case t: TermSymbol => t }.get
        assertEquals(ScalaSources.docFor(lone), Some("/** Doc for nested Lone. */"))
        assertEquals(ScalaSources.location(lone).map(l => (l.startLine, l.endLine)), Some((24, 26)))
        val module = lone.moduleClass.get
        assertEquals(methodSig(module, "g"), "def g(a: Int = 4): Int")
      }
    }

  test("sources: a written companion apply wins over the case class's synthetic one"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val companion = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarCase")
        val apply     = decls(companion, "apply").head
        assertEquals(TypePrinter.printSymbolSignature(apply), "def apply(a: Int, b: String = \"custom\"): CellarCase")
        assertEquals(ScalaSources.docFor(apply), Some("/** Custom apply. */"))
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarCase")
        assertEquals(methodSig(cls, "<init>"), "def <init>(a: Int = 1, b: String = \"x\"): Unit")
      }
    }

  test("sources: secondary constructors take their own defaults"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarSecondary")
        val sigs = decls(cls, "<init>").map(TypePrinter.printSymbolSignature).sorted
        assertEquals(sigs, List("def <init>(a: Int): Unit", "def <init>(s: String, b: Int = 5): Unit"))
      }
    }

  test("sources: a class with a symbolic name is found through its encoded classfile"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.~>")
        assertEquals(methodSig(cls, "m"), "def m(a: Int = 6): Int")
        assertEquals(methodSig(cls, "<init>"), "def <init>(x: Int = 5): Unit")
        assertEquals(ScalaSources.docFor(cls), Some("/** Symbolic class. */"))
      }
    }

  test("sources: a case class field has the position of its constructor parameter"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarADT")
        val ab    = decls(cls, "CellarAB").collectFirst { case c: ClassSymbol => c }.get
        val value = decls(ab, "value").head
        assertEquals(ScalaSources.location(value).map(_.startLine), Some(7))
      }
    }

  test("sources: Scala 3 top-level, extension and enum-case defaults"):
    withSourcesCtx(TestFixtures.scala3Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val holder = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarSources$package")
        assertEquals(methodSig(holder, "topLevelDefault"), "def topLevelDefault(a: Int, b: Int = 7): Int")
        assertEquals(methodSig(holder, "plusDefault"), "def plusDefault(x: Int)(y: Int = 1): Int")
        val color  = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarColor")
        val custom = decls(color, "Custom").collectFirst { case c: ClassSymbol => c }.get
        assertEquals(methodSig(custom, "<init>"), "def <init>(r: Int = 255): Unit")
      }
    }

  test("printSymbolSignature marks parameters that have a default (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        assertEquals(sugarSig(fqn, "withDefault"), "def withDefault(a: Int, b: String = ...): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "def curriedDefault(a: Int)(b: Int = ...): Int")
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
