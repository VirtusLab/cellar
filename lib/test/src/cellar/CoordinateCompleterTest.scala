package cellar

import cats.effect.IO
import munit.CatsEffectSuite

class CoordinateCompleterTest extends CatsEffectSuite:

  test("suggest returns suggestions for partial artifact name"):
    val coord = MavenCoordinate("org.typelevel", "cats-cor", "2.0.0")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      assert(suggestions.nonEmpty, "Expected at least one suggestion")
      assert(
        suggestions.exists(_.contains("cats-core")),
        s"Expected a suggestion containing 'cats-core', got: $suggestions"
      )
    }

  test("suggest returns latest version hint for wrong version"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "9.9.9")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      assert(suggestions.nonEmpty, "Expected a version hint")
      assert(
        suggestions.head.startsWith("Artifact exists."),
        s"Expected 'Artifact exists.' hint, got: $suggestions"
      )
    }

  test("suggest returns empty for completely nonexistent coordinate"):
    val coord = MavenCoordinate("com.nonexistent.x12345", "foo", "1.0.0")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      assertEquals(suggestions, Nil)
    }

  test("suggest limits results to at most 5"):
    // scala3-library has many artifacts starting with "scala3-"
    val coord = MavenCoordinate("org.scala-lang", "scala3-", "3.3.0")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      assert(suggestions.size <= 5, s"Expected at most 5 suggestions, got ${suggestions.size}")
    }
