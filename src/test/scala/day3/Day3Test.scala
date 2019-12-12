package day3

import common.AoCExampleRunner
import day3.Day3.{ buildCoveredPath, findLowestDistanceFromOrigin, parseInput }

class Day3Test extends AoCExampleRunner {
  type Input = (Seq[Movement], Seq[Movement])
  type Output = Int

  val examples: Map[(Seq[Movement], Seq[Movement]), Int] = Map(
    ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") -> 159,
    ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") -> 135
  ).map { case ((w1, w2), v) =>
    (parseInput(w1.split(",").toSeq), parseInput(w2.split(",").toSeq)) -> v
  }

  def method: ((Seq[Movement], Seq[Movement])) => Int = { case (m1, m2) =>
    findLowestDistanceFromOrigin(buildCoveredPath(m1), buildCoveredPath(m2)).get
  }

  "process" should { behave like examplesSolver() }
}
