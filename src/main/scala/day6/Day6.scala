package day6

import cats.data.ReaderWriterState
import cats.kernel.instances.int._
import cats.instances.list.{catsKernelStdMonoidForList => _, _}
import cats.syntax.foldable._
import common.AoCRunnable

object Day6 extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 6

  def parseInput(lines: Array[String]): List[(String, String)] = {
    lines.map(_.split("\\)")).map { case Array(from, to) => from.stripLineEnd -> to.stripLineEnd }.toList
  }

  lazy val orbitalRelationships: Map[String, List[String]] = parseInput(inputLines().toArray).groupMap(_._1)(_._2)
  val COM: String = "COM"
  val YOU: String = "YOU"
  val SAN: String = "SAN"

  def numberOfOrbitsRec(relationships: Map[String, List[String]]): Output = {
    def aux(planet: String, level: Int): Output = {
      relationships.get(planet).fold(0)(_.foldMap(level + aux(_, level + 1)))
    }

    aux(COM, level = 1)
  }

  type RWS[A] = ReaderWriterState[Unit, Int, Int, A]

  def numberOfOrbitsFromYouToSanta(relationships: Map[String, List[String]]): Option[Output] = {
    val twoWayRelationships: Map[String, List[String]] = {
      relationships
        .toList
        .flatMap { case (from, tos) => tos.map(from -> _) ++ tos.map(_ -> from) }
        .groupMap(_._1)(_._2)
        .view.mapValues(_.toList)
        .toMap
    }

    def aux(planet: String, seen: Set[String]): Option[Output] = {
      twoWayRelationships.get(planet) match {
        case None => None
        case Some(l) if !l.contains(SAN) => l.filterNot(seen).flatMap(aux(_, seen + planet).map(_ + 1)).minOption
        case _ => Some(0)
      }
    }

    aux(YOU, Set()).map(_ - 1)
  }

  override lazy val part1: Option[Int] = Some {
    numberOfOrbitsRec(orbitalRelationships)
  }

  override lazy val part2: Option[Int] = {
    numberOfOrbitsFromYouToSanta(orbitalRelationships)
  }
}
