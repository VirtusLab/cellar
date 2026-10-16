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

  test("printSymbolSignature prefixes class and object modifiers (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = (fqn: String) => TypePrinter.printSymbolSignature(ctx.findStaticClass(fqn))
        assert(sig("cellar.fixture.scala3.CellarADT").startsWith("sealed trait CellarADT"))
        assert(sig("cellar.fixture.scala3.CellarA").startsWith("final case class CellarA "))
        assertEquals(TypePrinter.printSymbolSignature(ctx.findStaticTerm("cellar.fixture.scala3.CellarB")), "case object CellarB")
      }
    }

  test("printSymbolSignature prefixes class, object and implicit modifiers (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val adt = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarADT").declarations
        val sigs = adt.filter(d => d.name.toString.startsWith("CellarA")).map(TypePrinter.printSymbolSignature)
        assert(sigs.contains("case object CellarAA"), sigs.mkString("\n"))
        assert(sigs.exists(_.startsWith("final case class CellarAB ")), sigs.mkString("\n"))
        val instances = ctx.findStaticClass("cellar.fixture.scala2.CellarInstances")
        val intInstance = instances.declarations.find(_.name.toString == "intInstance").get
        assertEquals(TypePrinter.printSymbolSignature(intInstance), "implicit val intInstance: CellarTypeClass[Int]")
      }
    }

  test("printSymbolSignature prints context bounds as written"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val renderAll = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarTC").declarations
          .find(_.name.toString == "renderAll").get

        // when
        val sig = TypePrinter.printSymbolSignature(renderAll)

        // then
        assertEquals(sig, "def renderAll[A: CellarTC, B](as: List[A], b: B)(using sep: String): String")
      }
    }

  test("printSymbolSignature prints an anonymous using parameter as its type"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val separated = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarTC").declarations
          .find(_.name.toString == "separated").get

        // when
        val sig = TypePrinter.printSymbolSignature(separated)

        // then
        assertEquals(sig, "def separated(as: List[String])(using String): String")
      }
    }

  test("printSymbolSignature prints context bounds as written (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val describe = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarTypeClass").declarations
          .find(_.name.toString == "describe").get

        // when
        val sig = TypePrinter.printSymbolSignature(describe)

        // then
        assertEquals(sig, "def describe[A: CellarTypeClass](a: A): String")
      }
    }

  test("printSymbolSignature leaves out parents the compiler adds to objects"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val companion = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarPoint")

        // when
        val sig = TypePrinter.printSymbolSignature(companion)

        // then
        assertEquals(sig, "object CellarPoint")
      }
    }

  test("printSymbolSignature prints a singleton type with its .type"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val self = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarPoint").declarations
          .find(_.name.toString == "Self").get

        // when
        val sig = TypePrinter.printSymbolSignature(self)

        // then
        assertEquals(sig, "type Self = CellarPoint.type")
      }
    }

  test("printSymbolSignature names an object scope as the object"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val origin = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarPoint").declarations
          .find(_.name.toString == "origin").get

        // when
        val sig = TypePrinter.printSymbolSignature(origin)

        // then
        assertEquals(sig, "private[CellarPoint] def origin: Int")
      }
    }

  test("printSymbolSignature marks an implicit class implicit (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val ops = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarSyntax").declarations
          .collectFirst { case c: tastyquery.Symbols.ClassSymbol if c.name.toString == "CellarOps" => c }.get

        // when
        val sig = TypePrinter.printSymbolSignature(ops)

        // then
        assertEquals(sig, "implicit class CellarOps[A]")
      }
    }

  test("printSymbolSignature keeps a Serializable parent written by hand"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val obj = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarSerializable")

        // when
        val sig = TypePrinter.printSymbolSignature(obj)

        // then
        assertEquals(sig, "object CellarSerializable extends Serializable")
      }
    }

  test("printSymbolSignature keeps a Serializable parent written by hand (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val obj = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarSerializable")

        // when
        val sig = TypePrinter.printSymbolSignature(obj)

        // then
        assertEquals(sig, "object CellarSerializable extends Serializable")
      }
    }

  test("printSymbolSignature leaves out parents the compiler adds to objects (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val companion = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarTypeClass")

        // when
        val sig = TypePrinter.printSymbolSignature(companion)

        // then
        assertEquals(sig, "object CellarTypeClass")
      }
    }

  test("printSymbolSignature renders lazy vals and vars (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val decls = ctx.findStaticClass("cellar.fixture.scala3.CellarModifiers").declarations
        val sig   = (name: String) => TypePrinter.printSymbolSignature(decls.find(_.name.toString == name).get)
        assertEquals(sig("lazyValue"), "lazy val lazyValue: Int")
        assertEquals(sig("mutable"), "var mutable: Int")
        assertEquals(sig("guarded"), "protected def guarded: Int")
        assertEquals(sig("internal"), "private[scala3] def internal: Int")
      }
    }

  test("printSymbolSignature renders lazy vals, vars and implicit objects (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val decls = ctx.findStaticClass("cellar.fixture.scala2.CellarModifiers").declarations
        val sigs  = decls.map(TypePrinter.printSymbolSignature)
        assert(sigs.contains("lazy val lazyValue: Int"), sigs.mkString("\n"))
        assert(sigs.contains("var mutable: Int"), sigs.mkString("\n"))
        assert(sigs.contains("implicit object ImplicitObject"), sigs.mkString("\n"))
        assert(sigs.contains("protected def guarded: Int"), sigs.mkString("\n"))
        assert(sigs.contains("private[scala2] def internal: Int"), sigs.mkString("\n"))
      }
    }

  test("printSymbolSignature prints the statics object of a Java class as the class itself"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val module = ctx.findStaticModuleClass("java.util.AbstractList")
        assertEquals(
          TypePrinter.printSymbolSignature(module),
          "abstract class AbstractList<E> extends AbstractCollection<E> implements List<E>"
        )
      }
    }

  test("printSymbolSignature renders an enum and its cases as declared (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        assertEquals(TypePrinter.printSymbolSignature(ctx.findStaticClass("cellar.fixture.scala3.CellarShape")), "enum CellarShape")
        val cases = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarShape").declarations
        val sig   = (name: String) => TypePrinter.printSymbolSignature(cases.find(_.name.toString == name).get)
        assertEquals(sig("Circle"), "case Circle extends CellarShape")
        assertEquals(sig("Polygon"), "case Polygon extends CellarShape")
      }
    }

  test("printSymbolSignature does not mark a constructor abstract (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val polygon = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarShape").declarations
          .collectFirst { case c: tastyquery.Symbols.ClassSymbol if c.name.toString == "Polygon" => c }.get
        val ctor = polygon.declarations.find(_.name.toString == "<init>").get
        assertEquals(TypePrinter.printSymbolSignature(ctor), "def <init>(sides: Int): Unit")
      }
    }

  test("printSymbolSignature renders a Java enum as enum"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.java.CellarJavaEnum")
        assertEquals(TypePrinter.printSymbolSignature(cls), "enum CellarJavaEnum")
      }
    }

  test("printSymbolSignature prefixes abstract and final modifiers (Java)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = (fqn: String) => TypePrinter.printSymbolSignature(ctx.findStaticClass(fqn))
        assert(sig("java.util.AbstractList").startsWith("abstract class AbstractList"))
        assert(sig("java.lang.String").startsWith("final class String"))
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
        assertEquals(sig, "abstract def combine[A, B](a: A)(b: B): B")
      }
    }

  test("printSymbolSignature renders unbounded type params and curried lists (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala2.CellarCurried")
        val combine = cls.declarations.find(_.name.toString == "combine").get
        val sig     = TypePrinter.printSymbolSignature(combine)
        assertEquals(sig, "abstract def combine[A, B](a: A)(b: B): B")
      }
    }

  test("printSymbolSignature renders higher-kinded type params (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val wrap = cls.declarations.find(_.name.toString == "wrap").get
        val sig  = TypePrinter.printSymbolSignature(wrap)
        assertEquals(sig, "abstract def wrap[F[_], A](fa: F[A]): F[A]")
      }
    }

  test("printSymbolSignature keeps a higher-kinded type param's lower bound"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val widen = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded").declarations
          .find(_.name.toString == "widen").get

        // when
        val sig = TypePrinter.printSymbolSignature(widen)

        // then
        assertEquals(sig, "abstract def widen[F2[x] >: List[x]](fa: F2[Int]): F2[Int]")
      }
    }

  test("printSymbolSignature keeps a refinement's members"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val refined = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded").declarations
          .find(_.name.toString == "refined").get

        // when
        val sig = TypePrinter.printSymbolSignature(refined)

        // then
        assertEquals(sig, "abstract def refined: CellarBox[List] { type Elem = Int; def first: Int }")
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
          "abstract def compose[F[_], G[_]](bf: CellarBox[F], bg: CellarBox[G]): CellarBox[[A] =>> F[G[A]]]"
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
        assertEquals(TypePrinter.printSymbolSignature(alias), "type HandWrittenEta[A] = List[A]")
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
          "abstract def composeBounded[F[_]](bf: CellarBox[F]): CellarBoundedBox[[A <: AnyRef] =>> F[A]]"
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
        assertEquals(sig, "abstract def bounded[F[_ <: AnyRef]](fa: F[String]): F[String]")
      }
    }

  test("printSymbolSignature renders a constructor-bounded higher-kinded type param (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val upper = cls.declarations.find(_.name.toString == "upper").get
        val sig   = TypePrinter.printSymbolSignature(upper)
        assertEquals(sig, "abstract def upper[F[A] <: Iterable[A]](fa: F[Int]): F[Int]")
      }
    }

  test("printSymbolSignature renders a self-referential higher-kinded param bound (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls         = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val selfBounded = cls.declarations.find(_.name.toString == "selfBounded").get
        val sig         = TypePrinter.printSymbolSignature(selfBounded)
        assertEquals(sig, "abstract def selfBounded[F[A <: Comparable[A]]](fa: F[String]): F[String]")
      }
    }

  test("printSymbolSignature renders a multi-arity higher-kinded type param (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val bimap = cls.declarations.find(_.name.toString == "bimap").get
        val sig   = TypePrinter.printSymbolSignature(bimap)
        assertEquals(sig, "abstract def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]")
      }
    }

  test("printSymbolSignature renders higher-kinded type params (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val wrap = cls.declarations.find(_.name.toString == "wrap").get
        val sig  = TypePrinter.printSymbolSignature(wrap)
        assertEquals(sig, "abstract def wrap[F[_], A](fa: F[A]): F[A]")
      }
    }

  test("printSymbolSignature renders a bounded higher-kinded type param (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val bounded = cls.declarations.find(_.name.toString == "bounded").get
        val sig     = TypePrinter.printSymbolSignature(bounded)
        assertEquals(sig, "abstract def bounded[F[_ <: AnyRef]](fa: F[String]): F[String]")
      }
    }

  test("printSymbolSignature renders a multi-arity higher-kinded type param (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val bimap = cls.declarations.find(_.name.toString == "bimap").get
        val sig   = TypePrinter.printSymbolSignature(bimap)
        assertEquals(sig, "abstract def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]")
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
        assertEquals(sig, "class CellarJavaClass<T extends Comparable<T>> implements CellarJavaInterface<T>")
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
        assertEquals(sig, "boolean equals(Object anObject)")
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
            "String format(String value)",
            "String format(int value)",
            "String format(int value, boolean verbose)"
          )
        )
        assertEquals(sugarSig(fqn, "repeat"), "List<T> repeat(T value, int times)")
        assertEquals(sugarSig(fqn, "<init>"), "CellarJavaClass(T defaultValue)")
        val module = ctx.findStaticModuleClass(fqn)
        val of     = module.declarations.find(_.name.toString == "of").get
        assertEquals(TypePrinter.printSymbolSignature(of), "static <E extends Comparable<E>> CellarJavaClass<E> of(E value)")
      }
    }

  test("printSymbolSignature names Java parameters typed by another class's nested type"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = ctx.findStaticClass("java.util.AbstractMap.SimpleEntry").declarations
          .filter(_.name.toString == "<init>").map(TypePrinter.printSymbolSignature)
        assert(sig.contains("SimpleEntry(Map.Entry<? extends K, ? extends V> entry)"), sig)
      }
    }

  test("printSymbolSignature names a static nested Java interface's parameters from the sources jar"):
    withSourcesCtx(TestFixtures.javaCoord) { ctx =>
      IO.blocking {
        // given
        given Context = ctx

        // when
        val sig = sugarSig("cellar.fixture.java.CellarJavaNested.Callback", "call")

        // then
        assertEquals(sig, "void call(String message)")
      }
    }

  test("printSymbolSignature names abstract Java parameters from the sources jar"):
    withSourcesCtx(TestFixtures.javaCoord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.java.CellarJavaInterface"
        assertEquals(sugarSig(fqn, "identity"), "T identity(T value)")
        assertEquals(sugarSig(fqn, "repeat"), "List<T> repeat(T value, int times)")
      }
    }

  test("printSymbolSignature renders Java generics, wildcards and varargs in Java syntax"):
    withJavaCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val statics = ctx.findStaticModuleClass("java.util.Collections").declarations

        // when
        val sigs = statics.filter(s => Set("addAll", "max").contains(s.name.toString)).map(TypePrinter.printSymbolSignature)

        // then
        assert(sigs.contains("static <T> boolean addAll(Collection<? super T> c, T... elements)"), sigs)
        assert(sigs.contains("static <T extends Object & Comparable<? super T>> T max(Collection<? extends T> coll)"), sigs)
      }
    }

  test("printSymbolSignature renders Java interface, enum and annotation declarations"):
    withJavaCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val pkg = "cellar.fixture.java"

        // when
        val enumSig       = TypePrinter.printSymbolSignature(ctx.findStaticClass(s"$pkg.CellarJavaEnum"))
        val annotationSig = TypePrinter.printSymbolSignature(ctx.findStaticClass(s"$pkg.CellarJavaAnnotation"))
        val describeSig   = sugarSig(s"$pkg.CellarJavaInterface", "describe")

        // then
        assertEquals(enumSig, "enum CellarJavaEnum")
        assertEquals(annotationSig, "@interface CellarJavaAnnotation")
        assertEquals(describeSig, "default String describe()")
      }
    }

  test("printSymbolSignature qualifies a static class nested two deep with every outer class"):
    withJavaCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val statics = ctx.findStaticModuleClass("cellar.fixture.java.CellarJavaNested").declarations

        // when
        val sig = TypePrinter.printSymbolSignature(statics.find(_.name.toString == "inner").get)

        // then
        assertEquals(sig, "static CellarJavaNested.Mid.Inner inner()")
      }
    }

  test("printSymbolSignature prints what Java source declares where the classfile adds to it"):
    withJavaCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val fqn      = "cellar.fixture.java.CellarJavaNested"
        def sigs(cls: tastyquery.Symbols.ClassSymbol, name: String) =
          cls.declarations.filter(_.name.toString == name).map(TypePrinter.printSymbolSignature)
        val nested   = ctx.findStaticClass(fqn)
        val statics  = ctx.findStaticModuleClass(fqn)
        def nestedClass(in: tastyquery.Symbols.ClassSymbol, name: String) =
          in.declarations.collectFirst { case c: tastyquery.Symbols.ClassSymbol if c.name.toString == name => c }.get
        val bound    = nestedClass(nested, "Bound")
        val hidden   = nestedClass(statics, "Hidden")

        // when
        val constructors      = sigs(nested, "<init>")
        val closing           = sigs(statics, "closing")
        val innerConstructors = sigs(bound, "<init>")

        // then
        assert(constructors.contains("<T> CellarJavaNested(T value, int count)"), constructors)
        assert(constructors.contains("CellarJavaNested(String... names)"), constructors)
        assertEquals(TypePrinter.printSymbolSignature(hidden), "protected static class Hidden")
        assertEquals(closing, List("static <C extends AutoCloseable> C closing(C closeable)"))
        assertEquals(innerConstructors, List("Bound(int size)"))
        assertEquals(sigs(nestedClass(nested, "Linked"), "<init>"), List("Linked(CellarJavaNested owner, List<E> items)"))
        assertEquals(sigs(nestedClass(statics, "Box"), "<init>"), List("<U> Box(T value, U extra)"))
      }
    }

  test("printSymbolSignature prints a Java member in Scala syntax when asked to"):
    withJavaCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val repeat = ctx.findStaticClass("cellar.fixture.java.CellarJavaClass").declarations
          .find(_.name.toString == "repeat").get

        // when
        val sig = TypePrinter.printSymbolSignature(repeat, DetectedLanguage.Scala3)

        // then
        assertEquals(sig, "def repeat(value: T, times: Int): List[T]")
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
        assertEquals(sugarSig(fqn, "transform"), "abstract def transform[A, B](f: A => B): B")
        assertEquals(sugarSig(fqn, "zip"), "abstract def zip[A, B, C](f: (A, B) => C): C")
        assertEquals(sugarSig(fqn, "nested"), "abstract def nested[A, B, C](f: (A => B) => C): C")
        assertEquals(sugarSig(fqn, "thunk"), "abstract def thunk[A](f: () => A): A")
        assertEquals(sugarSig(fqn, "ctx"), "abstract def ctx[A, B](f: A ?=> B): B")
      }
    }

  test("printSymbolSignature renders tuple types as paren sugar (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "pair"), "abstract def pair[A, B](t: (A, B)): (A, B)")
        assertEquals(sugarSig(fqn, "triple"), "abstract def triple[A, B, C](t: (A, B, C)): (A, B, C)")
        assertEquals(sugarSig(fqn, "tupleArg"), "abstract def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean")
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
        assertEquals(sig, "abstract def mapK[F[_], G[_]](f: F ~> G): Unit")
      }
    }

  test("printSymbolSignature collapses trivial wildcard bounds (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "wildcard")
        assertEquals(sig, "abstract def wildcard: List[?]")
      }
    }

  test("printSymbolSignature keeps a real wildcard upper bound (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "boundedWildcard")
        assertEquals(sig, "abstract def boundedWildcard: List[? <: AnyRef]")
      }
    }

  test("printSymbolSignature renders implicit and using param lists (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "withImplicit"), "abstract def withImplicit[A](a: A)(implicit s: CellarShow[A]): A")
        assertEquals(sugarSig(fqn, "withUsing"), "abstract def withUsing[A](a: A)(using s: CellarShow[A]): A")
      }
    }

  test("printSymbolSignature marks parameters that have a default (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "withDefault"), "abstract def withDefault(a: Int, b: String = ...): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "abstract def curriedDefault(a: Int)(b: Int = ...): Int")
      }
    }

  test("printSymbolSignature prints default values from the sources jar (Scala 3)"):
    withSourcesCtx(TestFixtures.scala3Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        assertEquals(sugarSig(fqn, "withDefault"), "abstract def withDefault(a: Int, b: String = \"b\"): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "abstract def curriedDefault(a: Int)(b: Int = a): Int")
      }
    }

  test("printSymbolSignature prints default values from the sources jar (Scala 2)"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        assertEquals(sugarSig(fqn, "withDefault"), "abstract def withDefault(a: Int, b: String = \"b\"): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "abstract def curriedDefault(a: Int)(b: Int = a): Int")
      }
    }

  test("printSymbolSignature takes defaults from the source file of the symbol's own package"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        assertEquals(
          sugarSig("cellar.fixture.scala2.CellarSugar", "withDefault"),
          "abstract def withDefault(a: Int, b: String = \"b\"): String"
        )
        assertEquals(
          sugarSig("cellar.fixture.scala2.nested.CellarSugar", "withDefault"),
          "abstract def withDefault(a: Int, b: String = \"nested\"): String"
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
        assertEquals(byType("abstract def process(value: Int): String"), (Some("/** Int process. */"), Some(5)))
        assertEquals(byType("abstract def process(value: String): String"), (Some("/** String process. */"), Some(7)))
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
        assertEquals(sugarSig(fqn, "withDefault"), "abstract def withDefault(a: Int, b: String = ...): String")
        assertEquals(sugarSig(fqn, "curriedDefault"), "abstract def curriedDefault(a: Int)(b: Int = ...): Int")
      }
    }

  test("printSymbolSignature renders function types as arrow sugar (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        assertEquals(sugarSig(fqn, "transform"), "abstract def transform[A, B](f: A => B): B")
        assertEquals(sugarSig(fqn, "zip"), "abstract def zip[A, B, C](f: (A, B) => C): C")
        assertEquals(sugarSig(fqn, "nested"), "abstract def nested[A, B, C](f: (A => B) => C): C")
        assertEquals(sugarSig(fqn, "thunk"), "abstract def thunk[A](f: () => A): A")
      }
    }

  test("printSymbolSignature renders tuple types as paren sugar (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        assertEquals(sugarSig(fqn, "pair"), "abstract def pair[A, B](t: (A, B)): (A, B)")
        assertEquals(sugarSig(fqn, "triple"), "abstract def triple[A, B, C](t: (A, B, C)): (A, B, C)")
        assertEquals(sugarSig(fqn, "tupleArg"), "abstract def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean")
      }
    }

  test("printSymbolSignature collapses trivial wildcard bounds (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala2.CellarSugar", "wildcard")
        assertEquals(sig, "abstract def wildcard: List[?]")
      }
    }

  test("printSymbolSignature keeps a real wildcard upper bound (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala2.CellarSugar", "boundedWildcard")
        assertEquals(sig, "abstract def boundedWildcard: List[? <: AnyRef]")
      }
    }

  test("printSymbolSignature renders an implicit param list (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        assertEquals(
          sugarSig("cellar.fixture.scala2.CellarSugar", "withImplicit"),
          "abstract def withImplicit[A](a: A)(implicit s: CellarShow[A]): A"
        )
      }
    }
