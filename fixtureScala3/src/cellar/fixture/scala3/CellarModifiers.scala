package cellar.fixture.scala3

/** Fixture for modifiers printed on member lines. */
class CellarModifiers:
  lazy val lazyValue: Int = 1
  var mutable: Int = 0
  given Int = 0

/** Fixture for how an enum and its cases are printed. */
enum CellarShape:
  case Circle, Square
  case Polygon(sides: Int)
