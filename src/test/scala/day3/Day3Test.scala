package day3

import common.AoCExampleRunner
import day3.Day3._

import scala.collection.SeqMap

abstract class Day3Test extends AoCExampleRunner {
  type Input = (Seq[Movement], Seq[Movement])
  type Output = Int

  val solutions: List[Output]
  val method: ((Seq[Movement], Seq[Movement])) => Int

  val examples: SeqMap[(Seq[Movement], Seq[Movement]), Int] = List(
    ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"),
    ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
  ).map { case (w1, w2) =>
    parseInput(w1.split(",").toSeq) -> parseInput(w2.split(",").toSeq)
  }.zip(solutions).to(SeqMap)

  "process" should { behave like examplesSolver() }
}

class Day3Part1Test extends Day3Test {
  override lazy val solutions: List[Int] = List(159, 135)
  override val method: ((Seq[Movement], Seq[Movement])) => Int = { case (m1, m2) =>
    findClosestIntersection(buildCoveredPath(m1), buildCoveredPath(m2)).get
  }
}

class Day3Part2Test extends Day3Test {
  override lazy val solutions: List[Int] = List(610, 410)
  override val method: ((Seq[Movement], Seq[Movement])) => Int = { case (m1, m2) =>
    findEarliestIntersection(buildCoveredPath(m1), buildCoveredPath(m2)).get
  }
}
