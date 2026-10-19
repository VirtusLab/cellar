package cellar

object CellarErrorTest extends weaver.FunSuite:

  private val coord = MavenCoordinate("org.example", "lib", "1.0.0")

  test("CoordinateNotFound getMessage is non-empty and contains coord"):
    val cause = new RuntimeException("network error")
    val e     = CellarError.CoordinateNotFound(coord, cause)
    expect.all(
      e.getMessage.nonEmpty,
      e.getMessage.contains("org.example:lib:1.0.0")
    )

  test("CoordinateNotFound getCause returns the provided cause"):
    val cause = new RuntimeException("root")
    val e     = CellarError.CoordinateNotFound(coord, cause)
    expect(clue(e.getCause) == cause)

  test("SymbolNotFound with empty nearMatches produces no near-match text"):
    val e = CellarError.SymbolNotFound("Foo", coord, Nil)
    expect(!e.getMessage.contains("Did you mean"))

  test("SymbolNotFound with empty nearMatches and non-latest version suggests latest"):
    val e = CellarError.SymbolNotFound("Foo", coord, Nil)
    expect(e.getMessage.contains("org.example:lib:latest"))

  test("SymbolNotFound with empty nearMatches and version=latest does not add a Try hint"):
    val latestCoord = MavenCoordinate("org.example", "lib", "latest")
    val e           = CellarError.SymbolNotFound("Foo", latestCoord, Nil)
    expect(!e.getMessage.contains("Try 'org.example:lib:latest'"))

  test("SymbolNotFound with nearMatches lists all of them"):
    val e = CellarError.SymbolNotFound("Foo", coord, List("bar.Foo", "baz.Foo"))
    expect.all(
      e.getMessage.contains("bar.Foo"),
      e.getMessage.contains("baz.Foo")
    )

  test("SymbolNotFound getMessage contains the fqn"):
    val e = CellarError.SymbolNotFound("cats.Monad", coord, Nil)
    expect(e.getMessage.contains("cats.Monad"))

  test("PackageGivenToGet getMessage mentions 'cellar list'"):
    val e = CellarError.PackageGivenToGet("cats")
    expect.all(
      e.getMessage.contains("cellar list"),
      e.getMessage.contains("cats")
    )

  test("EmptyArtifact getMessage contains coord and explains missing symbols"):
    val e = CellarError.EmptyArtifact(coord)
    expect.all(
      e.getMessage.contains("org.example:lib:1.0.0"),
      e.getMessage.nonEmpty
    )

  test("ShadedDuplicate getMessage contains both JAR paths"):
    val p1 = fs2.io.file.Path("/tmp/a.jar")
    val p2 = fs2.io.file.Path("/tmp/b.jar")
    val e  = CellarError.ShadedDuplicate("com.Foo", p1, p2)
    expect.all(
      e.getMessage.contains("/tmp/a.jar"),
      e.getMessage.contains("/tmp/b.jar"),
      e.getMessage.contains("com.Foo")
    )

  test("CoordinateNotFound with suggestions renders 'Did you mean?'"):
    val cause = new RuntimeException("not found")
    val suggestions = List("org.example:lib_3:2.0.0", "org.example:lib_2.13:2.0.0")
    val e = CellarError.CoordinateNotFound(coord, cause, suggestions)
    expect.all(
      e.getMessage.contains("Did you mean?"),
      e.getMessage.contains("org.example:lib_3:2.0.0"),
      e.getMessage.contains("org.example:lib_2.13:2.0.0")
    )

  test("CoordinateNotFound with version hint renders without 'Did you mean?'"):
    val cause = new RuntimeException("not found")
    val suggestions = List("Artifact exists. Latest version: 2.13.0")
    val e = CellarError.CoordinateNotFound(coord, cause, suggestions)
    expect.all(
      e.getMessage.contains("Artifact exists. Latest version: 2.13.0"),
      !e.getMessage.contains("Did you mean?")
    )

  test("CoordinateNotFound with empty suggestions renders generic message"):
    val cause = new RuntimeException("not found")
    val e = CellarError.CoordinateNotFound(coord, cause)
    expect.all(
      e.getMessage.contains("Check that the group ID"),
      !e.getMessage.contains("Did you mean?")
    )

  test("SymbolLookupFailed getMessage contains fqn and exception type"):
    val cause = new RuntimeException("bad TASTy")
    val e     = CellarError.SymbolLookupFailed("org.foo.Bar$", cause)
    expect.all(
      e.getMessage.contains("org.foo.Bar$"),
      e.getMessage.contains("RuntimeException"),
      e.getMessage.contains("bad TASTy")
    )

  test("SymbolLookupFailed getMessage handles null cause message"):
    val cause = new NullPointerException()
    val e     = CellarError.SymbolLookupFailed("org.foo.Bar$", cause)
    expect.all(
      e.getMessage.contains("NullPointerException"),
      e.getMessage.contains("(no message)")
    )

  test("SymbolLookupFailed getCause returns the provided cause"):
    val cause = new RuntimeException("oops")
    val e     = CellarError.SymbolLookupFailed("org.foo.Bar$", cause)
    expect(clue(e.getCause) == cause)

  test("CellarError subtypes can be caught as CellarError"):
    val e: CellarError = CellarError.PackageGivenToGet("cats")
    matches(scala.util.Try(throw e)) { case scala.util.Failure(_: CellarError) => success }

  test("CellarError subtypes can be caught as Throwable"):
    val e: Throwable = CellarError.PackageGivenToGet("cats")
    matches(scala.util.Try(throw e)) { case scala.util.Failure(_: Throwable) => success }
