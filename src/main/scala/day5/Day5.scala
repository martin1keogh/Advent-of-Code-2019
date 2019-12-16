package day5

import intcode.AocRunnableInterpreter

object Day5 extends AocRunnableInterpreter {
  override val dayNumber: Int = 5

  override lazy val part1: Option[Int] = Some {
    runner.runA(program).value
  }
}
