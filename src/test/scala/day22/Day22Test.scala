package day22

import common.AoCExampleRunner
import day22.Day22._

import scala.collection.SeqMap

class Day22Test extends AoCExampleRunner {
  override type Input = List[String]
  override type Output = Deck
  override val examples: SeqMap[Input, Output] = SeqMap(
    """deal into new stack""" -> "9 8 7 6 5 4 3 2 1 0",
    """cut 3""" -> "3 4 5 6 7 8 9 0 1 2",
    """cut -4""" -> "6 7 8 9 0 1 2 3 4 5",
    """deal with increment 3""" -> "0 7 4 1 8 5 2 9 6 3",

    """deal with increment 7
      |deal into new stack
      |deal into new stack""".stripMargin -> "0 3 6 9 2 5 8 1 4 7",

    """cut 6
      |deal with increment 7
      |deal into new stack""".stripMargin -> "3 0 7 4 1 8 5 2 9 6",

    """deal with increment 7
      |deal with increment 9
      |cut -2""".stripMargin -> "6 3 0 7 4 1 8 5 2 9",

    """deal into new stack
      |cut -2
      |deal with increment 7
      |cut 8
      |cut -4
      |deal with increment 7
      |cut 3
      |deal with increment 9
      |deal with increment 3
      |cut -1""".stripMargin -> "9 2 5 8 1 4 7 0 3 6",
  ).map { case (input, output) =>
    input.split("\\n").map(_.stripLineEnd).toList -> output.split(" ").map(_.toInt).toVector
  }

  override def method: Input => Output = { input =>
    val tenCardDeck = Range(0, 10)
    applyTechniques(tenCardDeck.toVector, input.map(parseLine))
  }

  "process" should { behave like examplesSolver() }
}
