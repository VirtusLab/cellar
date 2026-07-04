package cellar.fixture.scala3

/** Fixture for signature rendering: function-type sugar + implicit/using lists. */
trait CellarShow[A]:
  def show(a: A): A

trait CellarSugar:
  def transform[A, B](f: A => B): B
  def zip[A, B, C](f: (A, B) => C): C
  def nested[A, B, C](f: (A => B) => C): C
  def thunk[A](f: () => A): A
  def ctx[A, B](f: A ?=> B): B
  def withImplicit[A](a: A)(implicit s: CellarShow[A]): A
  def withUsing[A](a: A)(using s: CellarShow[A]): A
