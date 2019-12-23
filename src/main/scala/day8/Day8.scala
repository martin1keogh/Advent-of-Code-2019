package day8

import common.AoCRunnable

object Day8 extends AoCRunnable{
  override type Output = Int
  override val dayNumber: Int = 8

  private val width = 25
  private val height = 6
  private val layerSize = width * height

  val layers: List[String] = inputLines().mkString.grouped(layerSize).toList

  override lazy val part1: Option[Int] = Some {
    val leastZeros = layers.minBy(_.count(_ == '0'))
    leastZeros.count(_ == '1') * leastZeros.count(_ == '2')
  }

  override lazy val part2: Option[Int] = {
    val perPixel: List[List[Char]] = layers.transpose.flatten.grouped(layers.length).toList
    val rows = perPixel.map(_.find(_ != '2').get).grouped(width).toList
    rows.foreach(row => println(row.map { case '0' => ' '; case '1' => '#' }.mkString))
    Some(0) // no 'computable' result this time
  }
}
