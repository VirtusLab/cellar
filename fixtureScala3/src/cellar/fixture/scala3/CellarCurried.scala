package cellar.fixture.scala3

/** Fixture for signature rendering: unbounded type params + curried parameter lists. */
trait CellarCurried:
  def combine[A, B](a: A)(b: B): B
