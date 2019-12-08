package day2

import common.AoCRunnable
import common.ChainOps

import scala.annotation.tailrec

object Main extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 2

  val program: Array[Int] = inputLines().next().split(",").map(_.toInt)

  def process(program: Array[Int]): Array[Int] = {
    @tailrec
    def subprocess(output: Array[Int], index: Int): Array[Int] = output.slice(index, index + 4) match {
      case Array(99, _*) => output

      case Array(opcode, input1Pos, input2Pos, outputPos) =>
        val input1 = output(input1Pos)
        val input2 = output(input2Pos)

        val outputValue = opcode match {
          case 1 => input1 + input2
          case 2 => input1 * input2
        }

        val updatedOutput = output.updated(outputPos, outputValue)
        subprocess(updatedOutput, index + 4)
    }

    subprocess(program, 0)
  }

  // spec: set position 1 to 12 and 2 to 2 before running program
  def restoreGravityProgram(program: Array[Int]): Array[Int] = {
    program
      .updated(1, 12)
      .updated(2, 2)
  }

  override val part1: Option[Int] = Some {
    program |> restoreGravityProgram |> process |> (_.head)
  }
}
