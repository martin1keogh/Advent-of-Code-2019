package common

import scala.io.Source

trait AoCRunnable {
  type Output
  val dayNumber: Int

  lazy val part1: Option[Output] = None
  lazy val part2: Option[Output] = None

  def inputLines(): Iterator[String] = Source.fromResource(s"day$dayNumber/input.data").getLines()

  def main(args: Array[String]): Unit = {
    part1.foreach(s1 => println(s"Solution to part1 of day $dayNumber: $s1"))
    part2.foreach(s2 => println(s"Solution to part2 of day $dayNumber: $s2"))
    ()
  }
}
