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
}
