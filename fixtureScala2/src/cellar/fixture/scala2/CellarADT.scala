package cellar.fixture.scala2

/** Sealed ADT with subtypes nested inside the companion object — the idiomatic Scala 2 ADT pattern. */
sealed trait CellarADT

object CellarADT {
  case object CellarAA extends CellarADT
  final case class CellarAB(value: Int) extends CellarADT
}
