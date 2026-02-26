package cellar

class CellarErrorTest extends munit.FunSuite:

  private val coord = MavenCoordinate("org.example", "lib", "1.0.0")

  test("CoordinateNotFound getMessage is non-empty and contains coord"):
    val cause = new RuntimeException("network error")
    val e     = CellarError.CoordinateNotFound(coord, cause)
    assert(e.getMessage.nonEmpty)
    assert(e.getMessage.contains("org.example:lib:1.0.0"))

  test("CoordinateNotFound getCause returns the provided cause"):
    val cause = new RuntimeException("root")
    val e     = CellarError.CoordinateNotFound(coord, cause)
    assertEquals(e.getCause, cause)

  test("SymbolNotFound with empty nearMatches produces no near-match text"):
    val e = CellarError.SymbolNotFound("Foo", coord, Nil)
    assert(!e.getMessage.contains("Did you mean"))

  test("SymbolNotFound with nearMatches lists all of them"):
    val e = CellarError.SymbolNotFound("Foo", coord, List("bar.Foo", "baz.Foo"))
    assert(e.getMessage.contains("bar.Foo"))
    assert(e.getMessage.contains("baz.Foo"))

  test("SymbolNotFound getMessage contains the fqn"):
    val e = CellarError.SymbolNotFound("cats.Monad", coord, Nil)
    assert(e.getMessage.contains("cats.Monad"))

  test("PackageGivenToGet getMessage mentions 'cellar list'"):
    val e = CellarError.PackageGivenToGet("cats")
    assert(e.getMessage.contains("cellar list"))
    assert(e.getMessage.contains("cats"))

  test("EmptyArtifact getMessage contains coord and explains missing symbols"):
    val e = CellarError.EmptyArtifact(coord)
    assert(e.getMessage.contains("org.example:lib:1.0.0"))
    assert(e.getMessage.nonEmpty)

  test("ShadedDuplicate getMessage contains both JAR paths"):
    val p1 = java.nio.file.Path.of("/tmp/a.jar")
    val p2 = java.nio.file.Path.of("/tmp/b.jar")
    val e  = CellarError.ShadedDuplicate("com.Foo", p1, p2)
    assert(e.getMessage.contains("/tmp/a.jar"))
    assert(e.getMessage.contains("/tmp/b.jar"))
    assert(e.getMessage.contains("com.Foo"))

  test("CoordinateNotFound with suggestions renders 'Did you mean?'"):
    val cause = new RuntimeException("not found")
    val suggestions = List("org.example:lib_3:2.0.0", "org.example:lib_2.13:2.0.0")
    val e = CellarError.CoordinateNotFound(coord, cause, suggestions)
    assert(e.getMessage.contains("Did you mean?"))
    assert(e.getMessage.contains("org.example:lib_3:2.0.0"))
    assert(e.getMessage.contains("org.example:lib_2.13:2.0.0"))

  test("CoordinateNotFound with version hint renders without 'Did you mean?'"):
    val cause = new RuntimeException("not found")
    val suggestions = List("Artifact exists. Latest version: 2.13.0")
    val e = CellarError.CoordinateNotFound(coord, cause, suggestions)
    assert(e.getMessage.contains("Artifact exists. Latest version: 2.13.0"))
    assert(!e.getMessage.contains("Did you mean?"))

  test("CoordinateNotFound with empty suggestions renders generic message"):
    val cause = new RuntimeException("not found")
    val e = CellarError.CoordinateNotFound(coord, cause)
    assert(e.getMessage.contains("Check that the group ID"))
    assert(!e.getMessage.contains("Did you mean?"))

  test("CellarError subtypes can be caught as CellarError"):
    val e: CellarError = CellarError.PackageGivenToGet("cats")
    intercept[CellarError](throw e)

  test("CellarError subtypes can be caught as Throwable"):
    val e: Throwable = CellarError.PackageGivenToGet("cats")
    intercept[Throwable](throw e)
