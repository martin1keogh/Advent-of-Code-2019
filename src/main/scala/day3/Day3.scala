package day3

import common.{ AoCRunnable, ChainOps }

object Day3 extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 3

  type CoveredPath = Seq[Point]

  val List(wire1, wire2) = inputLines().take(2).toList.map(_.split(","))

  def parseInput(wireInput: Seq[String]): Seq[Movement] = wireInput.map(Movement.parse)

  def buildCoveredPath(moves: Seq[Movement]): CoveredPath = {
    // construct the path by appending all visited points and prepending these to
    // a list. This way we can get the current point easily (head).
    // No real reason to keep the path for now, so we'll just convert the list
    // to a set at the end of the method
    moves.foldLeft(List(Point.origin)) {
      case (previousPath @ Point(x, y) :: _, move) =>
        val newlyCovered = move match {
          case Up(n)    => List.tabulate(n)(i => Point(x, y + i + 1))
          case Down(n)  => List.tabulate(n)(i => Point(x, y - i - 1))
          case Left(n)  => List.tabulate(n)(i => Point(x - i - 1, y))
          case Right(n) => List.tabulate(n)(i => Point(x + i + 1, y))
        }

        newlyCovered.reverse ::: previousPath

      // unreachable, use NEL here?
      case (Nil, _) => Nil
    }.reverse
  }

  def findClosestIntersection(pathWire1: CoveredPath, pathWire2: CoveredPath): Option[Int] = {
    // .tail call to remove Point.origin
    (pathWire1 intersect pathWire2).tail.map(_.manhattanDistanceFromOrigin).minOption
  }

  def findEarliestIntersection(pathWire1: CoveredPath, pathWire2: CoveredPath): Option[Int] = {
    // .tail call to remove Point.origin
    val intersections = (pathWire1 intersect pathWire2).tail
    intersections.map(p => pathWire1.indexOf(p) + pathWire2.indexOf(p)).minOption
  }

  private val coveredPaths = {
    val cp1 = wire1.toSeq |> parseInput |> buildCoveredPath
    val cp2 = wire2.toSeq |> parseInput |> buildCoveredPath
    (cp1, cp2)
  }

  override val part1: Option[Int] = {
    (findClosestIntersection _).tupled(coveredPaths)
  }

  override val part2: Option[Int] = {
    (findEarliestIntersection _).tupled(coveredPaths)
  }
}
