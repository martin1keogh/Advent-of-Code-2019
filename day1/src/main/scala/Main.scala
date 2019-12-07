package day1

import scala.io.Source

object Main extends App {
  val data = Source.fromResource("input.data")
  val masses = data.getLines().map(_.toInt)
  println(masses.map(_ / 3 - 2).sum)
}

