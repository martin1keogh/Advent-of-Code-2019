package intcode

import common.AoCExampleRunner
import intcode.Interpreter.runner

import scala.collection.immutable.SeqMap

class InterpreterTest extends AoCExampleRunner {
  type Input = Array[Int]
  type Output = Array[Int]

  val examples: SeqMap[Input, Output] = SeqMap(
    // day 2 related examples
    "1,0,0,0,99" -> "2,0,0,0,99",
    "2,3,0,3,99" -> "2,3,0,6,99",
    "2,4,4,5,99,0" -> "2,4,4,5,99,9801",
    "1,1,1,4,99,5,6,0,99" -> "30,1,1,4,2,5,6,0,99",

    // day 5
    "1002,4,3,4,33" -> "1002,4,3,4,99",
    "1101,100,-1,4,0" -> "1101,100,-1,4,99",
    "4,2,99,0" -> "4,2,99,0",
  ).map { case (input, output) =>
    input.split(",").map(_.toInt) -> output.split(",").map(_.toInt)
  }

  val method: Input => Output = { input =>
    runner.runS(Program(input)).value.underlying
  }

  "process" should { behave like examplesSolver() }
}
