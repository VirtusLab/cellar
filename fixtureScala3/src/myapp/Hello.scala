package myapp

@main def hello =
  println(42)

opaque type Hello = Int

object Hello:
  def fromInt(a: Int): Hello = a
