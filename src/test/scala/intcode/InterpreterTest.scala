package intcode

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.SeqMap

class InterpreterTest extends AnyWordSpec with Matchers with Interpreter {
  type Input = Array[Cell]
  type Output = Array[Cell]

  val examples: SeqMap[String, Seq[(Input, Output)]] = SeqMap(
    "day 2" -> Seq(
      "1,0,0,0,99" -> "2,0,0,0,99",
      "2,3,0,3,99" -> "2,3,0,6,99",
      "2,4,4,5,99,0" -> "2,4,4,5,99,9801",
      "1,1,1,4,99,5,6,0,99" -> "30,1,1,4,2,5,6,0,99",
    ),

    "day 5" -> Seq(
      "1002,4,3,4,33" -> "1002,4,3,4,99",
      "1101,100,-1,4,0" -> "1101,100,-1,4,99",
      "4,3,99,0" -> "4,3,99,0",
    ),

    "write instruction modes" -> Seq(
      "4,3,104,0,99" -> "4,3,104,0,99",
    ),

    "non-zero diagnostic code before exit" -> Seq(
      "104,1,99" -> "104,1,99"
    )
  ).view.mapValues(_.map { case (input, output) =>
    input.split(",").map(s => Cell(s.toInt)) -> output.split(",").map(s => Cell(s.toInt))
  }).to(SeqMap)

  val method: Input => Output = { input =>
    runner.runS(Program(input)).value._1.underlying
  }

  "Interpreter#runner" should afterWord("validate") {
    examples.foreach { case (exampleGroup, examples) =>
      exampleGroup should {
        examples.zipWithIndex.foreach { case ((input, output), index) =>
          s"example number ${index + 1}" in {
            method(input) should be(output)
          }
        }
      }
    }
  }
}
