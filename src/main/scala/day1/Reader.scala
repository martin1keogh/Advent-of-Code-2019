package day1

import scala.io.Source

trait Reader {
  val masses: Iterator[Int] = Source.fromResource("day1/input.data").getLines().map(_.toInt)
}
