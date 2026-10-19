package cellar

import cellar.handlers.SearchHandler
import weaver.FunSuite

object SearchHandlerTest extends FunSuite:

  private def sort(query: String, names: List[String]): List[String] =
    val lower = query.toLowerCase
    names.sortBy(SearchHandler.rankKey(lower, _))

  test("rankKey: exact match sorts before substring matches"):
    val sorted = sort("Monad", List("Bimonad", "Comonad", "Monad", "MonadError"))
    expect.eql("Monad", sorted.head)

  test("rankKey: exact match wins against longer prefix matches"):
    val sorted = sort("Map", List("MapLike", "Map", "MapOps"))
    expect.eql("Map", sorted.head)

  test("rankKey: case-insensitive exact match"):
    val sorted = sort("monad", List("Bimonad", "Monad", "Comonad"))
    expect.eql("Monad", sorted.head)

  test("rankKey: prefix matches outrank same-length non-prefix matches"):
    // Both "PreXYZ" and "XYZPre" contain "Pre" and are length 6;
    // the prefix match should win.
    val sorted = sort("Pre", List("XYZPre", "PreXYZ"))
    expect.eql(List("PreXYZ", "XYZPre"), sorted)

  test("rankKey: within equal rank, shorter names come first"):
    val sorted = sort("Foo", List("FooBarBaz", "FooBar"))
    expect.eql(List("FooBar", "FooBarBaz"), sorted)

  test("rankKey: within equal length, alphabetical"):
    val sorted = sort("Foo", List("FooC", "FooA", "FooB"))
    expect.eql(List("FooA", "FooB", "FooC"), sorted)

  test("rankKey: companion module ($-suffixed) sorts right after exact match"):
    val sorted = sort("Monad", List("MonadError", "Monad$", "Monad", "Bimonad"))
    expect.eql(List("Monad", "Monad$"), sorted.take(2))

  test("rankKey: FQN-style query ranks the exact full-name match first"):
    // FQN queries are matched/ranked against the symbol's full name.
    val sorted = sort("cats.FlatMap", List("cats.syntax.FlatMapOps", "cats.FlatMap", "cats.FlatMapArityFunctions"))
    expect.eql("cats.FlatMap", sorted.head)
