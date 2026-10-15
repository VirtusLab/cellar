package cellar.fixture.scala2

/** Fixture for matching declarations to the sources jar. */
trait CellarSources {
  /** Int process. */
  def process(value: Int): String
  /** String process. */
  def process(value: String): String

  /** Value Thing. */
  val Thing: Int
  /** Type Thing. */
  type Thing = Int

  def withBound[A: Ordering](a: A, b: Int = 3): Int = b

  def commented(xs: List[Int] = List(1, // one
    2)): Int = xs.sum

  /** Has a doc. */
  // an ordinary comment
  def afterComment(a: Int): Int = a

  /** Doc for nested Lone. */
  object Lone {
    def g(a: Int = 4): Int = a
  }
}

object CellarCase {
  /** Custom apply. */
  def apply(a: Int, b: String = "custom"): CellarCase = new CellarCase(a, b)
}
case class CellarCase(a: Int = 1, b: String = "x")

class CellarSecondary(a: Int) {
  def this(s: String, b: Int = 5) = this(b)
}

/** Symbolic class. */
class ~>(val x: Int = 5) {
  def m(a: Int = 6): Int = a
}
