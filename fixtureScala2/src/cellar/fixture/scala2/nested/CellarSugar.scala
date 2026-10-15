package cellar.fixture.scala2.nested

/** Same file name as cellar.fixture.scala2.CellarSugar, in a different package. */
trait CellarSugar {
  def withDefault(a: Int, b: String = "nested"): String
}
