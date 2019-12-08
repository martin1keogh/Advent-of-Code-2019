package day1

import scala.annotation.tailrec

object Part2 extends App with Reader {
  def computeFuelForMass(mass: Int): Int = {
    @tailrec
    def acc(currentMass: Int, subTotal: Int): Int = {
      val fuelForMass = currentMass / 3 - 2
      if (fuelForMass > 0) acc(fuelForMass, fuelForMass + subTotal)
      else subTotal
    }

    acc(mass, 0)
  }

  val total = masses.map(computeFuelForMass).sum
  println(total)
}
