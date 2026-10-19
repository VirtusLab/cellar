package cellar

import cats.effect.IO
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol}

object TypePrinterTest extends SequentialIOSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  private def withScala2Ctx[A](body: Context => IO[A]): IO[A] =
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala2Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  private def withSourcesCtx[A](coord: MavenCoordinate)(body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable *>
      JreClasspath.jrtPath().flatMap { jrePaths =>
        ContextResource.makeFromCoord(coord, jrePaths, Seq(TestFixtures.localM2Repo)).use { (ctx, _, _) => body(ctx) }
      }

  test("detectLanguage returns Scala3 for scala3 fixture symbol"):
    withCtx { ctx =>
      IO.blocking {
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val lang = TypePrinter.detectLanguage(cls)
        expect(clue(lang) == DetectedLanguage.Scala3)
      }
    }

  test("detectLanguage returns Java for java.lang.String"):
    withCtx { ctx =>
      IO.blocking {
        val cls  = ctx.findStaticClass("java.lang.String")
        val lang = TypePrinter.detectLanguage(cls)
        expect(clue(lang) == DetectedLanguage.Java)
      }
    }

  test("printSymbolSignature for trait contains 'trait' keyword"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val sig = TypePrinter.printSymbolSignature(cls)
        expect(sig.contains("trait"), s"Expected 'trait' in: $sig")
      }
    }

  test("printSymbolSignature for case class contains 'class' keyword"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val sig = TypePrinter.printSymbolSignature(cls)
        expect(sig.contains("class"), s"Expected 'class' in: $sig")
      }
    }

  test("printSymbolSignatureSafe prints a Scala 2 symbol without a language marker"):
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala2Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) =>
                  IO.blocking {
                    given Context = ctx
                    val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarTypeClass")
                    val sig = TypePrinter.printSymbolSignatureSafe(cls)
                    expect.eql("trait CellarTypeClass[A]", sig)
                  }
                }
    yield result

  test("printSymbolSignature for companion object term does not double the name"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val term = ctx.findStaticTerm("cellar.fixture.scala3.CellarTC")
        val sig  = TypePrinter.printSymbolSignature(term)
        expect.eql("object CellarTC", sig)
      }
    }

  test("printSymbolSignature for companion object class prints module class name"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarTC")
        val sig = TypePrinter.printSymbolSignature(cls)
        expect(sig.startsWith("object"), s"Expected 'object' keyword in: $sig")
      }
    }

  test("printSymbolSignature prefixes class and object modifiers (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = (fqn: String) => TypePrinter.printSymbolSignature(ctx.findStaticClass(fqn))
        expect(sig("cellar.fixture.scala3.CellarADT").startsWith("sealed trait CellarADT")) and
          expect(sig("cellar.fixture.scala3.CellarA").startsWith("final case class CellarA ")) and
          expect.eql("case object CellarB", TypePrinter.printSymbolSignature(ctx.findStaticTerm("cellar.fixture.scala3.CellarB")))
      }
    }

  test("printSymbolSignature prefixes class, object and implicit modifiers (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val adt = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarADT").declarations
        val sigs = adt.filter(d => d.name.toString.startsWith("CellarA")).map(TypePrinter.printSymbolSignature)
        val instances = ctx.findStaticClass("cellar.fixture.scala2.CellarInstances")
        val intInstance = instances.declarations.find(_.name.toString == "intInstance").get
        expect(sigs.contains("case object CellarAA"), sigs.mkString("\n")) and
          expect(sigs.exists(_.startsWith("final case class CellarAB ")), sigs.mkString("\n")) and
          expect.eql("implicit val intInstance: CellarTypeClass[Int]", TypePrinter.printSymbolSignature(intInstance))
      }
    }

  test("printSymbolSignature names an object scope as the object"):
    withCtx { ctx =>
      IO.blocking {
        // given
        given Context = ctx
        val origin = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarScope").declarations
          .find(_.name.toString == "origin").get

        // when
        val sig = TypePrinter.printSymbolSignature(origin)

        // then
        expect.eql("private[CellarScope] def origin: Int", sig)
      }
    }

  test("printSymbolSignature renders lazy vals and vars (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val decls = ctx.findStaticClass("cellar.fixture.scala3.CellarModifiers").declarations
        val sig   = (name: String) => TypePrinter.printSymbolSignature(decls.find(_.name.toString == name).get)
        expect.eql("lazy val lazyValue: Int", sig("lazyValue")) and
          expect.eql("var mutable: Int", sig("mutable")) and
          expect.eql("protected def guarded: Int", sig("guarded")) and
          expect.eql("private[scala3] def internal: Int", sig("internal"))
      }
    }

  test("printSymbolSignature renders lazy vals, vars and implicit objects (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val decls = ctx.findStaticClass("cellar.fixture.scala2.CellarModifiers").declarations
        val sigs  = decls.map(TypePrinter.printSymbolSignature)
        expect(sigs.contains("lazy val lazyValue: Int"), sigs.mkString("\n")) and
          expect(sigs.contains("var mutable: Int"), sigs.mkString("\n")) and
          expect(sigs.contains("implicit object ImplicitObject"), sigs.mkString("\n")) and
          expect(sigs.contains("protected def guarded: Int"), sigs.mkString("\n")) and
          expect(sigs.contains("private[scala2] def internal: Int"), sigs.mkString("\n"))
      }
    }

  test("printSymbolSignature prints the statics object of a Java class as the class itself"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val module = ctx.findStaticModuleClass("java.util.AbstractList")
        expect.eql(
          "abstract class AbstractList<E> extends AbstractCollection<E> implements List<E>",
          TypePrinter.printSymbolSignature(module)
        )
      }
    }

  test("printSymbolSignature renders an enum and its cases as declared (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cases = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarShape").declarations
        val sig   = (name: String) => TypePrinter.printSymbolSignature(cases.find(_.name.toString == name).get)
        expect.eql("enum CellarShape", TypePrinter.printSymbolSignature(ctx.findStaticClass("cellar.fixture.scala3.CellarShape"))) and
          expect.eql("case Circle extends CellarShape", sig("Circle")) and
          expect.eql("case Polygon extends CellarShape", sig("Polygon"))
      }
    }

  test("printSymbolSignature does not mark a constructor abstract (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val polygon = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarShape").declarations
          .collectFirst { case c: tastyquery.Symbols.ClassSymbol if c.name.toString == "Polygon" => c }.get
        val ctor = polygon.declarations.find(_.name.toString == "<init>").get
        expect.eql("def <init>(sides: Int): Unit", TypePrinter.printSymbolSignature(ctor))
      }
    }

  test("printSymbolSignature renders a Java enum as enum"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.java.CellarJavaEnum")
        expect.eql("enum CellarJavaEnum", TypePrinter.printSymbolSignature(cls))
      }
    }

  test("printSymbolSignature prefixes abstract and final modifiers (Java)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = (fqn: String) => TypePrinter.printSymbolSignature(ctx.findStaticClass(fqn))
        expect(sig("java.util.AbstractList").startsWith("abstract class AbstractList")) and
          expect(sig("java.lang.String").startsWith("final class String"))
      }
    }

  test("printSymbolSignatureSafe does not throw for Java symbol"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("java.lang.String")
        val sig = TypePrinter.printSymbolSignatureSafe(cls)
        expect(sig.nonEmpty)
      }
    }

  test("printSymbolSignature renders unbounded type params and curried lists (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarCurried")
        val combine = cls.declarations.find(_.name.toString == "combine").get
        val sig     = TypePrinter.printSymbolSignature(combine)
        expect.eql("abstract def combine[A, B](a: A)(b: B): B", sig)
      }
    }

  test("printSymbolSignature renders unbounded type params and curried lists (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala2.CellarCurried")
        val combine = cls.declarations.find(_.name.toString == "combine").get
        val sig     = TypePrinter.printSymbolSignature(combine)
        expect.eql("abstract def combine[A, B](a: A)(b: B): B", sig)
      }
    }

  test("printSymbolSignature renders higher-kinded type params (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val wrap = cls.declarations.find(_.name.toString == "wrap").get
        val sig  = TypePrinter.printSymbolSignature(wrap)
        expect.eql("abstract def wrap[F[_], A](fa: F[A]): F[A]", sig)
      }
    }

  test("printSymbolSignature renders higher-kinded class type params (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val box        = ctx.findStaticClass("cellar.fixture.scala3.CellarBox")
        val boundedBox = ctx.findStaticClass("cellar.fixture.scala3.CellarBoundedBox")
        expect.eql("trait CellarBox[F[_]]", TypePrinter.printSymbolSignature(box)) and
          expect.eql("trait CellarBoundedBox[F[_ <: AnyRef]]", TypePrinter.printSymbolSignature(boundedBox))
      }
    }

  test("printSymbolSignature renders a standalone type lambda as a type argument (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val compose = cls.declarations.find(_.name.toString == "compose").get
        val sig     = TypePrinter.printSymbolSignature(compose)
        expect.eql(
          "abstract def compose[F[_], G[_]](bf: CellarBox[F], bg: CellarBox[G]): CellarBox[[A] =>> F[G[A]]]",
          sig
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
        expect(!sig.contains("=>>"), s"eta-expansion not contracted: $sig") and
          expect.eql("class CellarSelfBox[A] extends CellarBox[CellarSelfBox]", sig)
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
        expect.eql("type HandWrittenEta = [A] =>> List[A]", TypePrinter.printSymbolSignature(alias))
      }
    }

  test("printSymbolSignature renders a bounded standalone type lambda as a type argument (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls           = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val composeBounded = cls.declarations.find(_.name.toString == "composeBounded").get
        val sig            = TypePrinter.printSymbolSignature(composeBounded)
        expect.eql(
          "abstract def composeBounded[F[_]](bf: CellarBox[F]): CellarBoundedBox[[A <: AnyRef] =>> F[A]]",
          sig
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
        expect.eql("abstract def bounded[F[_ <: AnyRef]](fa: F[String]): F[String]", sig)
      }
    }

  test("printSymbolSignature renders a constructor-bounded higher-kinded type param (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val upper = cls.declarations.find(_.name.toString == "upper").get
        val sig   = TypePrinter.printSymbolSignature(upper)
        expect.eql("abstract def upper[F[A] <: Iterable[A]](fa: F[Int]): F[Int]", sig)
      }
    }

  test("printSymbolSignature renders a self-referential higher-kinded param bound (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls         = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val selfBounded = cls.declarations.find(_.name.toString == "selfBounded").get
        val sig         = TypePrinter.printSymbolSignature(selfBounded)
        expect.eql("abstract def selfBounded[F[A <: Comparable[A]]](fa: F[String]): F[String]", sig)
      }
    }

  test("printSymbolSignature renders a multi-arity higher-kinded type param (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala3.CellarHigherKinded")
        val bimap = cls.declarations.find(_.name.toString == "bimap").get
        val sig   = TypePrinter.printSymbolSignature(bimap)
        expect.eql("abstract def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]", sig)
      }
    }

  test("printSymbolSignature renders higher-kinded type params (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val wrap = cls.declarations.find(_.name.toString == "wrap").get
        val sig  = TypePrinter.printSymbolSignature(wrap)
        expect.eql("abstract def wrap[F[_], A](fa: F[A]): F[A]", sig)
      }
    }

  test("printSymbolSignature renders a bounded higher-kinded type param (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val bounded = cls.declarations.find(_.name.toString == "bounded").get
        val sig     = TypePrinter.printSymbolSignature(bounded)
        expect.eql("abstract def bounded[F[_ <: AnyRef]](fa: F[String]): F[String]", sig)
      }
    }

  test("printSymbolSignature renders a multi-arity higher-kinded type param (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticClass("cellar.fixture.scala2.CellarHigherKinded")
        val bimap = cls.declarations.find(_.name.toString == "bimap").get
        val sig   = TypePrinter.printSymbolSignature(bimap)
        expect.eql("abstract def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]", sig)
      }
    }

  private def withJavaCtx[A](body: Context => IO[A]): IO[A] =
    for
      _        <- TestFixtures.assumeFixturesAvailable
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
        expect(!sig.contains("NothingType"), s"leaked internal type name: $sig") and
          expect.eql("class CellarJavaClass<T extends Comparable<T>> implements CellarJavaInterface<T>", sig)
      }
    }

  // Java's Object reaches tasty-query as a TypeRef named `<FromJavaObject>`, which used to print
  // verbatim in every Java signature that mentions Object.
  test("printSymbolSignature renders Java's Object as Object"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("java.lang.String", "equals")
        expect(!sig.contains("FromJavaObject"), s"leaked internal type name: $sig") and
          expect.eql("boolean equals(Object anObject)", sig)
      }
    }

  test("printSymbolSignature names Java parameters from LocalVariableTable when MethodParameters is absent"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.java.CellarJavaClass"
        val cls = ctx.findStaticClass(fqn)
        val formats = cls.declarations.filter(_.name.toString == "format").map(TypePrinter.printSymbolSignature).sorted
        val module = ctx.findStaticModuleClass(fqn)
        val of     = module.declarations.find(_.name.toString == "of").get
        expect.eql(
          List(
            "String format(String value)",
            "String format(int value)",
            "String format(int value, boolean verbose)"
          ),
          formats
        ) and
          expect.eql("List<T> repeat(T value, int times)", sugarSig(fqn, "repeat")) and
          expect.eql("CellarJavaClass(T defaultValue)", sugarSig(fqn, "<init>")) and
          expect.eql("static <E extends Comparable<E>> CellarJavaClass<E> of(E value)", TypePrinter.printSymbolSignature(of))
      }
    }

  test("printSymbolSignature names Java parameters typed by another class's nested type"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = ctx.findStaticClass("java.util.AbstractMap.SimpleEntry").declarations
          .filter(_.name.toString == "<init>").map(TypePrinter.printSymbolSignature)
        expect(sig.contains("SimpleEntry(Map.Entry<? extends K, ? extends V> entry)"), sig.mkString("\n"))
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
        expect.eql("void call(String message)", sig)
      }
    }

  test("printSymbolSignature names abstract Java parameters from the sources jar"):
    withSourcesCtx(TestFixtures.javaCoord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.java.CellarJavaInterface"
        expect.eql("T identity(T value)", sugarSig(fqn, "identity")) and
          expect.eql("List<T> repeat(T value, int times)", sugarSig(fqn, "repeat"))
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
        expect(sigs.contains("static <T> boolean addAll(Collection<? super T> c, T... elements)"), sigs.mkString("\n")) and
          expect(sigs.contains("static <T extends Object & Comparable<? super T>> T max(Collection<? extends T> coll)"), sigs.mkString("\n"))
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
        expect.eql("enum CellarJavaEnum", enumSig) and
          expect.eql("@interface CellarJavaAnnotation", annotationSig) and
          expect.eql("default String describe()", describeSig)
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
        expect.eql("static CellarJavaNested.Mid.Inner inner()", sig)
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
        expect(constructors.contains("<T> CellarJavaNested(T value, int count)"), constructors.mkString("\n")) and
          expect(constructors.contains("CellarJavaNested(String... names)"), constructors.mkString("\n")) and
          expect.eql("protected static class Hidden", TypePrinter.printSymbolSignature(hidden)) and
          expect.eql(List("static <C extends AutoCloseable> C closing(C closeable)"), closing) and
          expect.eql(List("Bound(int size)"), innerConstructors) and
          expect.eql(List("Linked(CellarJavaNested owner, List<E> items)"), sigs(nestedClass(nested, "Linked"), "<init>")) and
          expect.eql(List("<U> Box(T value, U extra)"), sigs(nestedClass(statics, "Box"), "<init>"))
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
        expect.eql("def repeat(value: T, times: Int): List[T]", sig)
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
        expect.eql("abstract def transform[A, B](f: A => B): B", sugarSig(fqn, "transform")) and
          expect.eql("abstract def zip[A, B, C](f: (A, B) => C): C", sugarSig(fqn, "zip")) and
          expect.eql("abstract def nested[A, B, C](f: (A => B) => C): C", sugarSig(fqn, "nested")) and
          expect.eql("abstract def thunk[A](f: () => A): A", sugarSig(fqn, "thunk")) and
          expect.eql("abstract def ctx[A, B](f: A ?=> B): B", sugarSig(fqn, "ctx"))
      }
    }

  test("printSymbolSignature renders tuple types as paren sugar (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        expect.eql("abstract def pair[A, B](t: (A, B)): (A, B)", sugarSig(fqn, "pair")) and
          expect.eql("abstract def triple[A, B, C](t: (A, B, C)): (A, B, C)", sugarSig(fqn, "triple")) and
          expect.eql("abstract def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean", sugarSig(fqn, "tupleArg"))
      }
    }

  test("printSymbolSignature parenthesises a function-typed parent in extends position"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("scala.PartialFunction")
        val sig = TypePrinter.printSymbolSignature(cls)
        expect(sig.contains("extends (A => B)"), s"Expected parenthesised function parent in: $sig")
      }
    }

  test("printSymbolSignature renders symbolic binary types as infix (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "mapK")
        expect.eql("abstract def mapK[F[_], G[_]](f: F ~> G): Unit", sig)
      }
    }

  test("printSymbolSignature collapses trivial wildcard bounds (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "wildcard")
        expect.eql("abstract def wildcard: List[?]", sig)
      }
    }

  test("printSymbolSignature keeps a real wildcard upper bound (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala3.CellarSugar", "boundedWildcard")
        expect.eql("abstract def boundedWildcard: List[? <: AnyRef]", sig)
      }
    }

  test("printSymbolSignature renders implicit and using param lists (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        expect.eql("abstract def withImplicit[A](a: A)(implicit s: CellarShow[A]): A", sugarSig(fqn, "withImplicit")) and
          expect.eql("abstract def withUsing[A](a: A)(using s: CellarShow[A]): A", sugarSig(fqn, "withUsing"))
      }
    }

  test("printSymbolSignature marks parameters that have a default (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        expect.eql("abstract def withDefault(a: Int, b: String = ...): String", sugarSig(fqn, "withDefault")) and
          expect.eql("abstract def curriedDefault(a: Int)(b: Int = ...): Int", sugarSig(fqn, "curriedDefault"))
      }
    }

  test("printSymbolSignature prints default values from the sources jar (Scala 3)"):
    withSourcesCtx(TestFixtures.scala3Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala3.CellarSugar"
        expect.eql("abstract def withDefault(a: Int, b: String = \"b\"): String", sugarSig(fqn, "withDefault")) and
          expect.eql("abstract def curriedDefault(a: Int)(b: Int = a): Int", sugarSig(fqn, "curriedDefault"))
      }
    }

  test("printSymbolSignature prints default values from the sources jar (Scala 2)"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        expect.eql("abstract def withDefault(a: Int, b: String = \"b\"): String", sugarSig(fqn, "withDefault")) and
          expect.eql("abstract def curriedDefault(a: Int)(b: Int = a): Int", sugarSig(fqn, "curriedDefault"))
      }
    }

  test("printSymbolSignature takes defaults from the source file of the symbol's own package"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        expect.eql(
          "abstract def withDefault(a: Int, b: String = \"b\"): String",
          sugarSig("cellar.fixture.scala2.CellarSugar", "withDefault")
        ) and
          expect.eql(
            "abstract def withDefault(a: Int, b: String = \"nested\"): String",
            sugarSig("cellar.fixture.scala2.nested.CellarSugar", "withDefault")
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
        expect(clue(byType("abstract def process(value: Int): String")) == (Some("/** Int process. */"), Some(5))) and
          expect(clue(byType("abstract def process(value: String): String")) == (Some("/** String process. */"), Some(7)))
      }
    }

  test("sources: a val and a type of the same name keep their own Scaladoc"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        val docs = decls(cls, "Thing").map(s => s.isType -> ScalaSources.docFor(s)).toMap
        expect.eql(Map(false -> Some("/** Value Thing. */"), true -> Some("/** Type Thing. */")), docs)
      }
    }

  test("sources: context-bound evidence does not stop a method from matching its source"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        expect(methodSig(cls, "withBound").contains("b: Int = 3"), methodSig(cls, "withBound"))
      }
    }

  test("sources: context bounds and anonymous using clauses match in Scala 3"):
    withSourcesCtx(TestFixtures.scala3Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala3.CellarScala3Sources")
        expect(methodSig(cls, "withBound").contains("b: Int = 3"), methodSig(cls, "withBound")) and
          expect(methodSig(cls, "withUsingDefault").contains("a: Int = 2"), methodSig(cls, "withUsingDefault"))
      }
    }

  test("sources: a multi-line default with a comment is elided rather than flattened"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        expect.eql("def commented(xs: List[Int] = ...): Int", methodSig(cls, "commented"))
      }
    }

  test("sources: a Scaladoc is found past an ordinary comment"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        expect.eql(Some("/** Has a doc. */"), ScalaSources.docFor(decls(cls, "afterComment").head))
      }
    }

  test("sources: an object nested in a trait is found through its module val"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarSources")
        val lone = decls(cls, "Lone").collectFirst { case t: TermSymbol => t }.get
        val module = lone.moduleClass.get
        expect.eql(Some("/** Doc for nested Lone. */"), ScalaSources.docFor(lone)) and
          expect(clue(ScalaSources.location(lone).map(l => (l.startLine, l.endLine))) == Some((24, 26))) and
          expect.eql("def g(a: Int = 4): Int", methodSig(module, "g"))
      }
    }

  test("sources: a written companion apply wins over the case class's synthetic one"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val companion = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarCase")
        val apply     = decls(companion, "apply").head
        val cls = ctx.findStaticClass("cellar.fixture.scala2.CellarCase")
        expect.eql("def apply(a: Int, b: String = \"custom\"): CellarCase", TypePrinter.printSymbolSignature(apply)) and
          expect.eql(Some("/** Custom apply. */"), ScalaSources.docFor(apply)) and
          expect.eql("def <init>(a: Int = 1, b: String = \"x\"): Unit", methodSig(cls, "<init>"))
      }
    }

  test("sources: secondary constructors take their own defaults"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala2.CellarSecondary")
        val sigs = decls(cls, "<init>").map(TypePrinter.printSymbolSignature).sorted
        expect.eql(List("def <init>(a: Int): Unit", "def <init>(s: String, b: Int = 5): Unit"), sigs)
      }
    }

  test("sources: a class with a symbolic name is found through its encoded classfile"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls = ctx.findStaticClass("cellar.fixture.scala2.~>")
        expect.eql("def m(a: Int = 6): Int", methodSig(cls, "m")) and
          expect.eql("def <init>(x: Int = 5): Unit", methodSig(cls, "<init>")) and
          expect.eql(Some("/** Symbolic class. */"), ScalaSources.docFor(cls))
      }
    }

  test("sources: a case class field has the position of its constructor parameter"):
    withSourcesCtx(TestFixtures.scala2Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val cls   = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarADT")
        val ab    = decls(cls, "CellarAB").collectFirst { case c: ClassSymbol => c }.get
        val value = decls(ab, "value").head
        expect.eql(Some(7), ScalaSources.location(value).map(_.startLine))
      }
    }

  test("sources: Scala 3 top-level, extension and enum-case defaults"):
    withSourcesCtx(TestFixtures.scala3Coord) { ctx =>
      IO.blocking {
        given Context = ctx
        val holder = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarSources$package")
        val color  = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarColor")
        val custom = decls(color, "Custom").collectFirst { case c: ClassSymbol => c }.get
        expect.eql("def topLevelDefault(a: Int, b: Int = 7): Int", methodSig(holder, "topLevelDefault")) and
          expect.eql("def plusDefault(x: Int)(y: Int = 1): Int", methodSig(holder, "plusDefault")) and
          expect.eql("def <init>(r: Int = 255): Unit", methodSig(custom, "<init>"))
      }
    }

  test("printSymbolSignature marks parameters that have a default (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        expect.eql("abstract def withDefault(a: Int, b: String = ...): String", sugarSig(fqn, "withDefault")) and
          expect.eql("abstract def curriedDefault(a: Int)(b: Int = ...): Int", sugarSig(fqn, "curriedDefault"))
      }
    }

  test("printSymbolSignature renders function types as arrow sugar (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        expect.eql("abstract def transform[A, B](f: A => B): B", sugarSig(fqn, "transform")) and
          expect.eql("abstract def zip[A, B, C](f: (A, B) => C): C", sugarSig(fqn, "zip")) and
          expect.eql("abstract def nested[A, B, C](f: (A => B) => C): C", sugarSig(fqn, "nested")) and
          expect.eql("abstract def thunk[A](f: () => A): A", sugarSig(fqn, "thunk"))
      }
    }

  test("printSymbolSignature renders tuple types as paren sugar (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val fqn = "cellar.fixture.scala2.CellarSugar"
        expect.eql("abstract def pair[A, B](t: (A, B)): (A, B)", sugarSig(fqn, "pair")) and
          expect.eql("abstract def triple[A, B, C](t: (A, B, C)): (A, B, C)", sugarSig(fqn, "triple")) and
          expect.eql("abstract def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean", sugarSig(fqn, "tupleArg"))
      }
    }

  test("printSymbolSignature collapses trivial wildcard bounds (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala2.CellarSugar", "wildcard")
        expect.eql("abstract def wildcard: List[?]", sig)
      }
    }

  test("printSymbolSignature keeps a real wildcard upper bound (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val sig = sugarSig("cellar.fixture.scala2.CellarSugar", "boundedWildcard")
        expect.eql("abstract def boundedWildcard: List[? <: AnyRef]", sig)
      }
    }

  test("printSymbolSignature renders an implicit param list (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        expect.eql(
          "abstract def withImplicit[A](a: A)(implicit s: CellarShow[A]): A",
          sugarSig("cellar.fixture.scala2.CellarSugar", "withImplicit")
        )
      }
    }
