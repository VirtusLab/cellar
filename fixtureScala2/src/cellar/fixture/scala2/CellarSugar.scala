package cellar.fixture.scala2

/** Fixture for signature rendering: function-type sugar + implicit lists. */
trait CellarShow[A] {
  def show(a: A): A
}

trait CellarSugar {
  def transform[A, B](f: A => B): B
  def zip[A, B, C](f: (A, B) => C): C
  def nested[A, B, C](f: (A => B) => C): C
  def thunk[A](f: () => A): A
  def withImplicit[A](a: A)(implicit s: CellarShow[A]): A
}
