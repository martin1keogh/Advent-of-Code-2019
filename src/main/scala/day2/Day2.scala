package day2

import common.AoCRunnable
import intcode.Interpreter.{ ProgramOutput, runner }
import intcode.{ Pointer, Program }

object Day2 extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 2

  val program: Program = Program(inputLines().next().split(",").map(_.toInt))

  def setNounAndVerb(noun: Int, verb: Int)(program: Program): Program = {
    program.setValue(Pointer(1), noun).setValue(Pointer(2), verb)
  }

  def processFor(noun: Int, verb: Int): ProgramOutput = {
    runner
      .contramap[Program](setNounAndVerb(noun, verb))
      .runA(program)
      .value
  }

  override lazy val part1: Option[Int] = Some {
    processFor(12, 2)
  }

  override lazy val part2: Option[Int] = {
    Iterator
      .tabulate(99, 99) { case (noun, verb) => (noun, verb) -> processFor(noun, verb) }
      .flatten
      .collectFirst { case ((noun, verb), 19690720) => noun * 100 + verb }
  }
}
