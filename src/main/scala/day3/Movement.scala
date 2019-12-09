package day3

sealed trait Movement {
  def amount: Int
}

object Movement {
  def parse(s: String): Movement = {
    val (direction, lengthAsString) = s.splitAt(1)
    val length = lengthAsString.toInt
    direction match {
      case "U" => Up(length)
      case "D" => Down(length)
      case "L" => Left(length)
      case "R" => Right(length)
    }
  }
}

case class Up(amount: Int) extends Movement
case class Down(amount: Int) extends Movement
case class Left(amount: Int) extends Movement
case class Right(amount: Int) extends Movement
