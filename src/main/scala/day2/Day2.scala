package day2

import intcode.{ AocRunnableInterpreter, Cell, Pointer, Program }

object Day2 extends AocRunnableInterpreter {
  override val dayNumber: Int = 2

  def setNounAndVerb(noun: Int, verb: Int)(program: Program): Program = {
    program.setValueAt(Pointer(1), Cell(noun)).setValueAt(Pointer(2), Cell(verb))
  }

  val getResult: ((Program, Pointer)) => Int = { case (program, _) =>
    program.underlying.head.value
  }

  def processFor(noun: Int, verb: Int): Int = {
    runner
      .contramap[Program](setNounAndVerb(noun, verb))
      .modify(getResult)
      .runS(program)
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
