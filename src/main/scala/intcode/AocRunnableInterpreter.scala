package intcode

import common.AoCRunnable

trait AocRunnableInterpreter extends AoCRunnable with Interpreter {
  type Output = Int
  lazy val program: Program = Program(inputLines().next().split(",").map(s => Cell(s.toInt)))
}
