package cellar.fixture.scala2

/** Fixture for signature rendering: higher-kinded type parameters. */
trait CellarHigherKinded {
  def wrap[F[_], A](fa: F[A]): F[A]
  def bounded[F[X <: AnyRef]](fa: F[String]): F[String]
  def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]
}
