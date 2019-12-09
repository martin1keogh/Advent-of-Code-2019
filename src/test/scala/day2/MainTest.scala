package day2

import common.AoCExampleRunner
import day2.Main.process

class MainTest extends AoCExampleRunner {
  type Input = Array[Int]
  type Output = Array[Int]

  val examples = Map(
    "1,0,0,0,99" -> "2,0,0,0,99",
    "2,3,0,3,99" -> "2,3,0,6,99",
    "2,4,4,5,99,0" -> "2,4,4,5,99,9801",
    "1,1,1,4,99,5,6,0,99" -> "30,1,1,4,2,5,6,0,99"
  ).map { case (input, output) =>
    input.split(",").map(_.toInt) -> output.split(",").map(_.toInt)
  }

  def method: Array[Int] => Array[Int] = process

  "process" should { behave like examplesSolver() }
}
