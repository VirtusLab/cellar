package cellar

import weaver.SimpleIOSuite

object CoordinateCompleterTest extends SimpleIOSuite:

  test("suggest returns suggestions for partial artifact name"):
    val coord = MavenCoordinate("org.typelevel", "cats-cor", "2.0.0")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      expect(suggestions.nonEmpty, "Expected at least one suggestion") and
        expect(
          suggestions.exists(_.contains("cats-core")),
          s"Expected a suggestion containing 'cats-core', got: $suggestions"
        )
    }

  test("suggest returns latest version hint for wrong version"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "9.9.9")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      expect(suggestions.nonEmpty, "Expected a version hint") and
        expect(
          suggestions.headOption.exists(_.startsWith("Artifact exists.")),
          s"Expected 'Artifact exists.' hint, got: $suggestions"
        )
    }

  test("suggest returns empty for completely nonexistent coordinate"):
    val coord = MavenCoordinate("com.nonexistent.x12345", "foo", "1.0.0")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      expect.eql(Nil, suggestions)
    }

  test("suggest limits results to at most 5"):
    // scala3-library has many artifacts starting with "scala3-"
    val coord = MavenCoordinate("org.scala-lang", "scala3-", "3.3.0")
    CoordinateCompleter.suggest(coord, Seq.empty).map { suggestions =>
      expect(suggestions.size <= 5, s"Expected at most 5 suggestions, got ${suggestions.size}")
    }

  test("resolveLatest replaces 'latest' with an actual version"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "latest")
    coord.resolveLatest().map { resolved =>
      expect(resolved.version != "latest") and
        expect(resolved.version.nonEmpty) and
        expect.eql(coord.group, resolved.group) and
        expect.eql(coord.artifact, resolved.artifact)
    }

  test("resolveLatest is a no-op for concrete versions"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0")
    coord.resolveLatest().map { resolved =>
      expect(clue(resolved) == coord)
    }

  test("resolveLatest raises CoordinateNotFound for nonexistent artifact"):
    val coord = MavenCoordinate("com.nonexistent.x12345", "foo", "latest")
    coord.resolveLatest().attempt.map {
      case Left(_: CellarError.CoordinateNotFound) => success
      case Left(e)  => failure(s"Expected CoordinateNotFound, got: ${e.getClass}: ${e.getMessage}")
      case Right(_) => failure("Expected failure for nonexistent artifact")
    }
