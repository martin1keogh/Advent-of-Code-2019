package day4

import common.AoCExampleRunner
import day4.Day4.validPasswordPart2

class Day4Test extends AoCExampleRunner {
  override type Input = Int
  override type Output = Boolean
  override val examples: Map[Int, Boolean] = Map(
    111111 -> false,
    223450 -> false,
    123789 -> false,
    112233 -> true,
    111222 -> false,
    123444 -> false,
    111122 -> true,
  )

  override def method: Int => Boolean = validPasswordPart2

  "process" should { behave like examplesSolver() }
}
