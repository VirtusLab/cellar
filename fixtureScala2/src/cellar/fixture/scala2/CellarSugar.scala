package cellar.fixture.scala2

/** Fixture for signature rendering: function-type sugar + implicit lists. */
trait CellarShow[A] {
  def show(a: A): A
}

trait CellarSugar {
  def wildcard: List[_]
  def boundedWildcard: List[_ <: AnyRef]

  def transform[A, B](f: A => B): B
  def zip[A, B, C](f: (A, B) => C): C
  def nested[A, B, C](f: (A => B) => C): C
  def thunk[A](f: () => A): A
  def pair[A, B](t: (A, B)): (A, B)
  def triple[A, B, C](t: (A, B, C)): (A, B, C)
  def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean
  def withImplicit[A](a: A)(implicit s: CellarShow[A]): A
  def withDefault(a: Int, b: String = "b"): String
  def curriedDefault(a: Int)(b: Int = a): Int
}
