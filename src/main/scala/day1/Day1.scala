package day1

import common.AoCRunnable

import scala.annotation.tailrec

object Day1 extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 1

  // Returns the amount of fuel required directly for the input mass ONLY
  def computeFuelForMass(mass: Int): Int = {
    mass / 3 - 2
  }

  def computeTotalFuelForMass(mass: Int): Int = {
    @tailrec
    def acc(currentMass: Int, subTotal: Int): Int = {
      val fuelForMass = computeFuelForMass(currentMass)
      if (fuelForMass > 0) acc(fuelForMass, fuelForMass + subTotal)
      else subTotal
    }

    acc(mass, 0)
  }

  val masses: List[Int] = inputLines().map(_.toInt).toList

  override val part1: Option[Int] = Some(masses.map(computeFuelForMass).sum)
  override val part2: Option[Int] = Some(masses.map(computeTotalFuelForMass).sum)
}
