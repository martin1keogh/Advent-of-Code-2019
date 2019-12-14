package day4

import common.AoCRunnable

object Day4 extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 4

  val range = Range(165432, 707912)

  def validPasswordPart1(value: Int): Boolean = {
    val asList = value.toString.toList
    asList == asList.sorted && ('0' to '9').exists { i =>
      asList.containsSlice(List.fill(2)(i))
    }
  }

  def validPasswordPart2(value: Int): Boolean = {
    val asList = value.toString.toList
    asList == asList.sorted && ('0' to '9').exists { i =>
      asList.containsSlice(List.fill(2)(i)) && !asList.containsSlice(List.fill(3)(i))
    }
  }

  override val part1: Option[Int] = Some {
    range.count(validPasswordPart1)
  }

  override val part2: Option[Int] = Some {
    range.count(validPasswordPart2)
  }
}
