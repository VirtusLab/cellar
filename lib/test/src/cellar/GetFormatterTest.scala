package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context

class GetFormatterTest extends CatsEffectSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
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
        assert(output.contains("## "), s"Expected heading in: $output")
        assert(output.contains("trait"), s"Expected 'trait' in: $output")
      }
    }

  test("formatSymbol for sealed trait produces Known subtypes line"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("**Known subtypes:**"), s"Expected subtypes in: $output")
      }
    }

  test("formatSymbol for sealed trait lists CellarA in subtypes"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("CellarA"), s"Expected CellarA in subtypes: $output")
      }
    }

  test("formatSymbol for non-sealed class has no Known subtypes line"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val output = GetFormatter.formatSymbol(cls)
        assert(!output.contains("**Known subtypes:**"), s"Unexpected subtypes: $output")
      }
    }

  test("formatSymbol output contains a scala code fence"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("```scala"), s"Expected code fence in: $output")
      }
    }

  test("formatSymbol members includes declared method"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("render"), s"Expected 'render' in members: $output")
      }
    }

  test("formatSymbol members does not include Object methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        assert(!output.contains("notify"), s"Unexpected Object method in: $output")
        assert(!output.contains("finalize"), s"Unexpected Object method in: $output")
      }
    }

  test("formatGetResult separates multiple symbols with ---"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls1   = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val cls2   = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatGetResult("test", List(cls1, cls2))
        assert(output.contains("---"), s"Expected separator in: $output")
      }
    }

  test("formatGetResult for single symbol has no --- separator"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val output = GetFormatter.formatGetResult("test", List(cls))
        assert(!output.contains("---"), s"Unexpected separator: $output")
      }
    }

  test("formatSymbol members includes all overloaded methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarOverloaded")
        val output = GetFormatter.formatSymbol(cls)
        val processCount = output.linesIterator.count(_.contains("def process("))
        assertEquals(processCount, 3, s"Expected 3 process overloads in:\n$output")
      }
    }

  test("formatSymbol members includes inherited overloaded methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarOverloadedChild")
        val output = GetFormatter.formatSymbol(cls)
        val actionCount = output.linesIterator.count(_.contains("def action("))
        assertEquals(actionCount, 2, s"Expected 2 action overloads in:\n$output")
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

  private def withScala2Ctx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
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
        val formatCount = output.linesIterator.count(_.contains("def format("))
        assertEquals(formatCount, 3, s"Expected 3 format overloads in:\n$output")
      }
    }

  test("formatSymbol companion members include full signatures"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("**Companion members:**"), s"Expected companion section in: $output")
        assert(output.contains("def apply"), s"Expected 'def apply' signature in companion: $output")
        assert(output.contains("CellarTC[A]"), s"Expected return type in companion signature: $output")
      }
    }

  test("formatSymbol members includes all overloaded methods (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala2.CellarOverloaded")
        val output = GetFormatter.formatSymbol(cls)
        val processCount = output.linesIterator.count(_.contains("def process("))
        assertEquals(processCount, 3, s"Expected 3 process overloads in:\n$output")
      }
    }

  // Scala 2 ADT: sealed trait with subtypes nested inside the companion object.
  test("formatSymbol for Scala 2 sealed trait lists nested subtypes with clean names"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala2.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("**Known subtypes:**"), s"Expected subtypes section in:\n$output")
        // Names must use source-level dotted notation, not JVM $ encoding
        assert(output.contains("CellarADT.CellarAA"), s"Expected clean CellarAA name in:\n$output")
        assert(output.contains("CellarADT.CellarAB"), s"Expected clean CellarAB name in:\n$output")
        assert(!output.contains("CellarADT$"), s"Expected no JVM-mangled names in:\n$output")
      }
    }

  test("formatGetResult for Scala 2 sealed trait does not emit duplicate companion module class result"):
    withScala2Ctx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala2.CellarADT").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("cellar.fixture.scala2.CellarADT", syms)
          // Should have trait + object, but not the raw CellarADT$ module class as a third block
          val headingCount = output.linesIterator.count(_.startsWith("## cellar.fixture.scala2.CellarADT"))
          assertEquals(headingCount, 2, s"Expected exactly 2 headings (trait + object), got $headingCount in:\n$output")
        case other => fail(s"Expected Found, got $other")
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
          assert(output.contains("apply"), s"Expected 'apply' in standalone object output:\n$output")
        case other => fail(s"Expected Found, got $other")
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
        assertEquals(output.linesIterator.count(_.contains("<init>")), 1, s"Output:\n$output")
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
        assertEquals(TypePrinter.printSymbolSignature(trt), "trait CellarInstances")
        assertEquals(TypePrinter.printSymbolSignature(obj), "object CellarInstances extends CellarInstances")
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
        assert(!traitOut.contains("<init>"), s"Trait output:\n$traitOut")
        assert(!objOut.contains("<init>"), s"Object output:\n$objOut")
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
        assert(full.contains("midMethod"), s"Expected midMethod in full output: $full")
        assert(hidden.contains("leafMethod"), s"Expected leafMethod in hidden output: $hidden")
        assert(!hidden.contains("midMethod"), s"Unexpected midMethod in hidden output: $hidden")
      }
    }

  test("formatSymbol --group-inherited adds section headers by declaring class"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        // CellarLeaf → CellarMid → CellarOuter
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarLeaf")
        val output = GetFormatter.formatSymbol(cls, groupInherited = true)
        assert(output.contains("// Declared on CellarLeaf"), s"Expected declared section in:\n$output")
        assert(output.contains("// Inherited from CellarMid"), s"Expected CellarMid section in:\n$output")
        assert(output.contains("// Inherited from CellarOuter"), s"Expected CellarOuter section in:\n$output")
        assert(output.contains("leafMethod"), s"Expected leafMethod in:\n$output")
        assert(output.contains("midMethod"), s"Expected midMethod in:\n$output")
      }
    }

  test("formatSymbol --hide-inherited wins over --group-inherited"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarLeaf")
        val output = GetFormatter.formatSymbol(cls, hideInherited = true, groupInherited = true)
        assert(output.contains("leafMethod"), s"Expected leafMethod in:\n$output")
        assert(!output.contains("midMethod"), s"Unexpected midMethod in:\n$output")
        assert(!output.contains("Inherited from"), s"Unexpected section header in:\n$output")
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
        assert(fullCount >= 3, s"Expected >= 3 members in full, got $fullCount in:\n$full")
        // Limited output has exactly 2 member lines in the code block
        assert(limited.contains("… "), s"Expected truncation note in: $limited")
        assert(limited.contains("more members"), s"Expected 'more members' in: $limited")
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
          assert(!output.contains("$package$"), s"Expected no $$package$$ in:\n$output")
          assert(output.contains("## myapp.hello"), s"Expected '## myapp.hello' heading in:\n$output")
          assert(output.contains("**Origin:** myapp"), s"Expected origin 'myapp' in:\n$output")
          assert(output.contains("def hello: Unit"), s"Expected 'def hello: Unit' in:\n$output")
          // @main wrapper class is implementation detail — must not appear
          assert(!output.contains("class hello"), s"Unexpected '@main' wrapper class in:\n$output")
        case other => fail(s"Expected Found, got $other")
      }
    }

  test("formatGetResult for top-level opaque type hides the representation"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.Hello").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("myapp.Hello", syms)
          assert(!output.contains("$package$"), s"Expected no $$package$$ in:\n$output")
          // `Hello` is abstract outside myapp: `val h: Hello = 5` does not compile,
          // so the underlying Int must not be advertised as part of the API.
          assert(output.contains("type Hello"), s"Expected opaque-type signature in:\n$output")
          assert(!output.contains("= Int"), s"Opaque representation leaked in:\n$output")
          assert(!output.contains("symbol["), s"Unexpected raw symbol[] sentinel in:\n$output")
        case other => fail(s"Expected Found, got $other")
      }
    }

  test("formatGetResult for top-level companion member hides $package$"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.Hello.fromInt").map {
        case LookupResult.Found(syms) =>
          val output = GetFormatter.formatGetResult("myapp.Hello.fromInt", syms)
          assert(!output.contains("$package$"), s"Expected no $$package$$ in:\n$output")
          assert(output.contains("## myapp.Hello.fromInt"), s"Expected '## myapp.Hello.fromInt' heading in:\n$output")
          assert(output.contains("**Origin:** myapp.Hello"), s"Expected origin 'myapp.Hello' in:\n$output")
        case other => fail(s"Expected Found, got $other")
      }
    }
