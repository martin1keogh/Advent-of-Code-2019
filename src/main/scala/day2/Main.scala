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

  def setNounAndVerb(noun: Int, verb: Int)(program: Array[Int]): Array[Int] = {
    program.updated(1, noun).updated(2, verb)
  }

  def processFor(noun: Int, verb: Int): Int = {
    program |> setNounAndVerb(noun, verb) |> process |> (_.head)
  }

  override val part1: Option[Int] = Some {
    processFor(12, 2)
  }

  override val part2: Option[Int] = {
    Iterator
      .tabulate(99, 99) { case (noun, verb) => (noun, verb) -> processFor(noun, verb) }
      .flatten
      .collectFirst { case ((noun, verb), 19690720) => noun * 100 + verb }
  }
}
