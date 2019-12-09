package day3

case class Point(x: Int, y: Int) {
  val manhattanDistanceFromOrigin: Int = math.abs(x) + math.abs(y)
}

object Point {
  val origin: Point = Point(0, 0)
}
