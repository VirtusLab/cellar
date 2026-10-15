package cellar.fixture.scala3

def topLevelDefault(a: Int, b: Int = 7): Int = a + b

extension (x: Int) def plusDefault(y: Int = 1): Int = x + y

enum CellarColor:
  case Red, Green
  case Custom(r: Int = 255)

trait CellarScala3Sources:
  def withUsingDefault(a: Int = 2)(using Ordering[Int]): Int
  def withBound[A: Ordering](a: A, b: Int = 3): Int
