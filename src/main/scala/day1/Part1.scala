package day1

object Part1 extends App with Reader {
  println(masses.map(_ / 3 - 2).sum)
}
