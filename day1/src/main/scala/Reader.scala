package day1

import scala.io.Source

trait Reader {
  val masses: Iterator[Int] = Source.fromResource("input.data").getLines().map(_.toInt)
}
