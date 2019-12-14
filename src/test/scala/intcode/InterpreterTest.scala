package intcode

import common.AoCExampleRunner
import intcode.Interpreter.{ Program, runner }

import scala.collection.immutable.SeqMap

class InterpreterTest extends AoCExampleRunner {
  type Input = Program
  type Output = Program

  val examples: SeqMap[Program, Program] = SeqMap(
    "1,0,0,0,99" -> "2,0,0,0,99",
    "2,3,0,3,99" -> "2,3,0,6,99",
    "2,4,4,5,99,0" -> "2,4,4,5,99,9801",
    "1,1,1,4,99,5,6,0,99" -> "30,1,1,4,2,5,6,0,99"
  ).map { case (input, output) =>
    input.split(",").map(_.toInt) -> output.split(",").map(_.toInt)
  }

  def method: Program => Program = runner.runS(_).value

  "process" should { behave like examplesSolver() }
}