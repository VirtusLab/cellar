package cellar

import cats.effect.IO
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context

object GetFormatterTest extends SequentialIOSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("formatSymbol for trait produces ## heading and trait keyword"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        expect(output.contains("## "), s"Expected heading in: $output") and
          expect(output.contains("trait"), s"Expected 'trait' in: $output")
      }
    }

  test("formatSymbol lists every supertype, the compiler-added ones too"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        def supertypes(cls: tastyquery.Symbols.ClassSymbol) =
          GetFormatter.formatSymbol(cls).linesIterator.find(_.startsWith("**Supertypes:**"))
        val caseClass = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val companion = ctx.findStaticModuleClass("cellar.fixture.scala3.CellarA")
        expect.eql(
          Some("**Supertypes:** java.io.Serializable, scala.Product, scala.Equals, cellar.fixture.scala3.CellarADT"),
          supertypes(caseClass)
        ) and
          expect.eql(Some("**Supertypes:** scala.deriving.Mirror.Product, scala.deriving.Mirror"), supertypes(companion)) and
          expect.eql(None, supertypes(ctx.findStaticClass("cellar.fixture.scala3.CellarBox")))
      }
    }

  test("formatSymbol for sealed trait produces Known subtypes line"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        expect(output.contains("**Known subtypes:**"), s"Expected subtypes in: $output")
      }
    }

  test("formatSymbol for sealed trait lists CellarA in subtypes"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        expect(output.contains("CellarA"), s"Expected CellarA in subtypes: $output")
      }
    }

  test("formatSymbol for non-sealed class has no Known subtypes line"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val output = GetFormatter.formatSymbol(cls)
        expect(!output.contains("**Known subtypes:**"), s"Unexpected subtypes: $output")
      }
    }

  test("formatSymbol output contains a scala code fence"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        expect(output.contains("```scala"), s"Expected code fence in: $output")
      }
    }

  test("formatSymbol members includes declared method"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        expect(output.contains("render"), s"Expected 'render' in members: $output")
      }
    }

  test("formatSymbol members does not include Object methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        expect(!output.contains("notify"), s"Unexpected Object method in: $output") and
          expect(!output.contains("finalize"), s"Unexpected Object method in: $output")
      }
    }

  test("formatGetResult separates multiple symbols with ---"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls1   = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val cls2   = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatGetResult("test", List(cls1, cls2))
        expect(output.contains("---"), s"Expected separator in: $output")
      }
    }

  test("formatGetResult for single symbol has no --- separator"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val output = GetFormatter.formatGetResult("test", List(cls))
        expect(!output.contains("---"), s"Unexpected separator: $output")
      }
    }

  test("formatSymbol members includes all overloaded methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarOverloaded")
        val output = GetFormatter.formatSymbol(cls)
        val processCount = output.linesIterator.count(_.contains("def process("))
        expect(clue(processCount) == 3, s"Expected 3 process overloads in:\n$output")
      }
    }

  test("formatSymbol members includes inherited overloaded methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarOverloadedChild")
        val output = GetFormatter.formatSymbol(cls)
        val actionCount = output.linesIterator.count(_.contains("def action("))
        expect(clue(actionCount) == 2, s"Expected 2 action overloads in:\n$output")
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

  private def withScala2Ctx[A](body: Context => IO[A]): IO[A] =
    for
      _        <- TestFixtures.assumeFixturesAvailable
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala2Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("formatSymbol members includes all overloaded methods (Java)"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.java.CellarJavaClass")
        val output = GetFormatter.formatSymbol(cls)
        val formatCount = output.linesIterator.count(_.contains("String format("))
        expect(clue(formatCount) == 3, s"Expected 3 format overloads in:\n$output")
      }
    }

  test("formatSymbol companion members include full signatures"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        expect(output.contains("**Companion members:**"), s"Expected companion section in: $output") and
          expect(output.contains("def apply"), s"Expected 'def apply' signature in companion: $output") and
          expect(output.contains("CellarTC[A]"), s"Expected return type in companion signature: $output")
      }
    }

  test("formatSymbol members includes all overloaded methods (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala2.CellarOverloaded")
        val output = GetFormatter.formatSymbol(cls)
        val processCount = output.linesIterator.count(_.contains("def process("))
        expect(clue(processCount) == 3, s"Expected 3 process overloads in:\n$output")
      }
    }

  // Scala 2 ADT: sealed trait with subtypes nested inside the companion object.
  test("formatSymbol for Scala 2 sealed trait lists nested subtypes with clean names"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala2.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        expect(output.contains("**Known subtypes:**"), s"Expected subtypes section in:\n$output") and
          // Names must use source-level dotted notation, not JVM $ encoding
          expect(output.contains("CellarADT.CellarAA"), s"Expected clean CellarAA name in:\n$output") and
          expect(output.contains("CellarADT.CellarAB"), s"Expected clean CellarAB name in:\n$output") and
          expect(!output.contains("CellarADT$"), s"Expected no JVM-mangled names in:\n$output")
      }
    }

  test("formatGetResult collapses a companion module into its class, keeping the companion members"):
    withScala2Ctx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala2.CellarADT").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("cellar.fixture.scala2.CellarADT", syms)
          // The trait section already prints the companion's members, so a separate section for
          // the module class would repeat the whole API, mirrored.
          val headingCount = output.linesIterator.count(_.startsWith("## cellar.fixture.scala2.CellarADT"))
          expect(clue(headingCount) == 1, s"Expected the companion collapsed into one heading in:\n$output") and
            // Collapsing must not lose them: they move under the trait's "Companion members".
            expect(output.contains("CellarAA"), s"Expected companion member CellarAA in:\n$output") and
            expect(output.contains("CellarAB"), s"Expected companion member CellarAB in:\n$output") and
            expect(!output.contains("CellarADT$"), s"Expected no JVM-mangled names in:\n$output")
        case other => failure(s"Expected Found, got $other")
      }
    }

  test("formatGetResult keeps a module that has no companion class in the result"):
    withCtx { ctx =>
      given Context = ctx
      // `cats.implicits`-style: an object with no same-named class must still render.
      SymbolResolver.resolve("cellar.fixture.scala3.CellarTC").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("cellar.fixture.scala3.CellarTC", syms)
          expect(output.contains("## cellar.fixture.scala3.CellarTC"), s"module dropped:\n$output")
        case other => failure(s"Expected Found, got $other")
      }
    }

  test("formatSymbol for an object resolved via SymbolResolver shows its members"):
    withCtx { ctx =>
      given Context = ctx
      // Celsius is an opaque type plus its companion object (apply, toFahrenheit, value).
      // SymbolResolver represents the companion by its module class, so renderMembers
      // sees a ClassSymbol and the members survive.
      SymbolResolver.resolve("cellar.fixture.scala3.Celsius").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("cellar.fixture.scala3.Celsius", syms)
          expect(output.contains("apply"), s"Expected 'apply' in standalone object output:\n$output")
        case other => failure(s"Expected Found, got $other")
      }
    }

  test("formatSymbol keeps Java constructors but drops an object's <init>"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.java.CellarJavaClass")
        val output = GetFormatter.formatSymbol(cls)
        // A Java type has no companion `apply`, so the constructors are the only
        // listing that shows how it is built. CellarJavaClass declares exactly one,
        // and a constructor is never inherited, so exactly one must be listed.
        expect(clue(output.linesIterator.count(_.startsWith("CellarJavaClass("))) == 1, s"Output:\n$output")
      }
    }

  test("formatSymbol lists a method with defaults once, without its default getters (Scala 3)"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarSugar")
        val output = GetFormatter.formatSymbol(cls)
        val lines  = output.linesIterator.filter(_.contains("withDefault")).toList
        expect(clue(lines) == List("abstract def withDefault(a: Int, b: String = ...): String"), s"Output:\n$output") and
          expect(!output.contains("$default$"), s"Output:\n$output")
      }
    }

  test("formatSymbol lists a method with defaults once, without its default getters (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala2.CellarSugar")
        val output = GetFormatter.formatSymbol(cls)
        val lines  = output.linesIterator.filter(_.contains("withDefault")).toList
        expect(clue(lines) == List("abstract def withDefault(a: Int, b: String = ...): String"), s"Output:\n$output") and
          expect(!output.contains("$default$"), s"Output:\n$output")
      }
    }

  test("formatSymbol omits the universal parent from a Scala 2 signature"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        // `AnyRef` is how the universal parent prints for a Scala 2 symbol; a real
        // parent must survive alongside it being dropped.
        val trt = ctx.findStaticClass("cellar.fixture.scala2.CellarInstances")
        val obj = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarInstances")
        expect.eql("trait CellarInstances", TypePrinter.printSymbolSignature(trt)) and
          expect.eql("object CellarInstances extends CellarInstances", TypePrinter.printSymbolSignature(obj))
      }
    }

  test("formatSymbol drops <init> for a trait and for an object"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        // A trait has no user-callable constructor, and the object that mixes it in
        // must not inherit one into its member list either.
        val trt = ctx.findStaticClass("cellar.fixture.scala2.CellarInstances")
        val obj = ctx.findStaticModuleClass("cellar.fixture.scala2.CellarInstances")
        val traitOut = GetFormatter.formatSymbol(trt)
        val objOut   = GetFormatter.formatSymbol(obj)
        expect(!traitOut.contains("<init>"), s"Trait output:\n$traitOut") and
          expect(!objOut.contains("<init>"), s"Object output:\n$objOut")
      }
    }

  test("formatSymbol --hide-inherited shows only declared members"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        // CellarLeaf declares leafMethod; midMethod and innerMethod are inherited
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarLeaf")
        val full   = GetFormatter.formatSymbol(cls)
        val hidden = GetFormatter.formatSymbol(cls, hideInherited = true)
        expect(full.contains("midMethod"), s"Expected midMethod in full output: $full") and
          expect(hidden.contains("leafMethod"), s"Expected leafMethod in hidden output: $hidden") and
          expect(!hidden.contains("midMethod"), s"Unexpected midMethod in hidden output: $hidden")
      }
    }

  test("formatSymbol --group-inherited adds section headers by declaring class"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        // CellarLeaf → CellarMid → CellarOuter
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarLeaf")
        val output = GetFormatter.formatSymbol(cls, groupInherited = true)
        expect(output.contains("// Declared on CellarLeaf"), s"Expected declared section in:\n$output") and
          expect(output.contains("// Inherited from CellarMid"), s"Expected CellarMid section in:\n$output") and
          expect(output.contains("// Inherited from CellarOuter"), s"Expected CellarOuter section in:\n$output") and
          expect(output.contains("leafMethod"), s"Expected leafMethod in:\n$output") and
          expect(output.contains("midMethod"), s"Expected midMethod in:\n$output")
      }
    }

  test("formatSymbol --hide-inherited wins over --group-inherited"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarLeaf")
        val output = GetFormatter.formatSymbol(cls, hideInherited = true, groupInherited = true)
        expect(output.contains("leafMethod"), s"Expected leafMethod in:\n$output") and
          expect(!output.contains("midMethod"), s"Unexpected midMethod in:\n$output") and
          expect(!output.contains("Inherited from"), s"Unexpected section header in:\n$output")
      }
    }

  test("formatSymbol --limit caps member count and shows note"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarOverloaded")
        val full   = GetFormatter.formatSymbol(cls)
        val limited = GetFormatter.formatSymbol(cls, limit = Some(2))
        // Full output has at least 3 process overloads + unique
        val fullCount = full.linesIterator.count(l => l.contains("def process(") || l.contains("def unique"))
        expect(fullCount >= 3, s"Expected >= 3 members in full, got $fullCount in:\n$full") and
          // Limited output has exactly 2 member lines in the code block
          expect(limited.contains("… "), s"Expected truncation note in: $limited") and
          expect(limited.contains("more members"), s"Expected 'more members' in: $limited")
      }
    }

  test("formatSymbol --limit caps companion members too"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarWithCompanion")
        val full    = GetFormatter.formatSymbol(cls)
        val limited = GetFormatter.formatSymbol(cls, limit = Some(1))
        val companionOf = (out: String) => out.split("""\*\*Companion members:\*\*""")(1).takeWhile(_ != '#')
        expect(companionOf(full).linesIterator.count(_.startsWith("def ")) >= 1, s"Expected companion members in:\n$full") and
          expect(companionOf(limited).contains("more members"), s"Expected truncation note in companion section of:\n$limited")
      }
    }

  test("formatSymbol --group-inherited labels companion members by declaring type"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls      = ctx.findStaticClass("cellar.fixture.scala3.CellarWithCompanion")
        val grouped  = GetFormatter.formatSymbol(cls, groupInherited = true)
        val flat     = GetFormatter.formatSymbol(cls)
        val companionOf = (out: String) => out.split("""\*\*Companion members:\*\*""")(1).takeWhile(_ != '#')
        expect(companionOf(grouped).contains("// Declared on"), s"Expected section header in companion of:\n$grouped") and
          expect(!companionOf(flat).contains("// Declared on"), s"Unexpected section header in companion of:\n$flat")
      }
    }

  test("formatSymbol --hide-inherited wins over --group-inherited for companion members"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarWithCompanion")
        val output = GetFormatter.formatSymbol(cls, hideInherited = true, groupInherited = true)
        val companion = output.split("""\*\*Companion members:\*\*""")(1).takeWhile(_ != '#')
        expect(!companion.contains("// Declared on"), s"Unexpected section header in companion of:\n$output")
      }
    }

  // Scala-3 top-level decls live inside a synthetic `<file>$package$` wrapper
  // class. The wrapper must not leak into the rendered Markdown heading,
  // origin line, or signature. Fixture: fixtureScala3/src/myapp/Hello.scala
  //   package myapp
  //   @main def hello = println(42)
  //   opaque type Hello = Int
  //   object Hello:
  //     def fromInt(a: Int): Hello = a

  test("formatGetResult for @main top-level def hides $package$ and synthetic launcher class"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.hello").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("myapp.hello", syms)
          expect(!output.contains("$package$"), s"Expected no $$package$$ in:\n$output") and
            expect(output.contains("## myapp.hello"), s"Expected '## myapp.hello' heading in:\n$output") and
            expect(output.contains("**Origin:** myapp"), s"Expected origin 'myapp' in:\n$output") and
            expect(output.contains("def hello: Unit"), s"Expected 'def hello: Unit' in:\n$output") and
            // @main wrapper class is implementation detail — must not appear
            expect(!output.contains("class hello"), s"Unexpected '@main' wrapper class in:\n$output")
        case other => failure(s"Expected Found, got $other")
      }
    }

  test("formatGetResult for top-level opaque type renders 'opaque type X = ...'"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.Hello").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("myapp.Hello", syms)
          expect(!output.contains("$package$"), s"Expected no $$package$$ in:\n$output") and
            expect(output.contains("opaque type Hello = Int"), s"Expected opaque-type signature in:\n$output") and
            expect(!output.contains("symbol["), s"Unexpected raw symbol[] sentinel in:\n$output")
        case other => failure(s"Expected Found, got $other")
      }
    }

  test("formatGetResult for top-level companion member hides $package$"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.Hello.fromInt").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("myapp.Hello.fromInt", syms)
          expect(!output.contains("$package$"), s"Expected no $$package$$ in:\n$output") and
            expect(output.contains("## myapp.Hello.fromInt"), s"Expected '## myapp.Hello.fromInt' heading in:\n$output") and
            expect(output.contains("**Origin:** myapp.Hello"), s"Expected origin 'myapp.Hello' in:\n$output")
        case other => failure(s"Expected Found, got $other")
      }
    }

  test("formatSymbol puts Supertypes right below the signature, apart from the docstring"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val output = GetFormatter.formatSymbol(ctx.findStaticClass("cellar.fixture.scala3.CellarA"), docstring = Some("A case."))
        expect(
          output.contains(
            "```\n**Supertypes:** java.io.Serializable, scala.Product, scala.Equals, cellar.fixture.scala3.CellarADT\n\nA case."
          ),
          s"Expected Supertypes between signature and docstring in:\n$output"
        )
      }
    }

  test("formatSymbol lists the parents scalac adds to a case class's companion (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val output = GetFormatter.formatSymbol(ctx.findStaticModuleClass("cellar.fixture.scala2.CellarADT.CellarAB"))
        expect(
          output.contains("**Supertypes:** java.io.Serializable, scala.runtime.AbstractFunction1, scala.Function1\n"),
          s"Expected compiler-added parents in:\n$output"
        )
      }
    }

  test("formatSymbol lists a Java class's interfaces as supertypes, without Matchable"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val output = GetFormatter.formatSymbol(ctx.findStaticClass("cellar.fixture.java.CellarJavaClass"))
        expect(output.contains("**Supertypes:** cellar.fixture.java.CellarJavaInterface\n"), s"Output:\n$output") and
          expect(!output.contains("Matchable"), s"Output:\n$output")
      }
    }
