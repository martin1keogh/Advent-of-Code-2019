package day6

import common.AoCExampleRunner
import day6.Day6._

import scala.collection.SeqMap

class Day6Test extends AoCExampleRunner {
  override type Input = List[(String, String)]
  override type Output = Int
  override val examples: SeqMap[Input, Output] = SeqMap(
    """COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |K)YOU
      |I)SAN""".stripMargin.split("\\n") -> 4
  ).map { case (k, v) =>
    parseInput(k) -> v
  }

  override def method: Input => Output = input => numberOfOrbitsFromYouToSanta(input.groupMap(_._1)(_._2)).get

  "process" should { behave like examplesSolver() }
}
