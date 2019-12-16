package day5

import intcode.{ AocRunnableInterpreter, Pointer, Program }

object Day5 extends AocRunnableInterpreter {
  override val dayNumber: Int = 5

  val getDiagnosticCode: ((Program, Pointer)) => Option[Int] = { case (finishedProgram, _) =>
    finishedProgram.lastDiagnosticCode.map(_.value)
  }

  override lazy val part1: Option[Int] = {
    runner.modify(getDiagnosticCode).runS(program).value
  }
}
