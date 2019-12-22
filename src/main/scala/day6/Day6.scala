package day6

import cats.data.ReaderWriterState
import cats.kernel.instances.int._
import cats.instances.list.{catsKernelStdMonoidForList => _, _}
import cats.syntax.monad._
import cats.syntax.traverse._
import common.AoCRunnable

object Day6 extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 6

  def parseInput(lines: Array[String]): List[(String, String)] = {
    lines.map(_.split("\\)")).map { case Array(from, to) => from.stripLineEnd -> to.stripLineEnd }.toList
  }

  lazy val orbitalRelationships: Map[String, List[String]] = parseInput(inputLines().toArray).groupMap(_._1)(_._2)
  val COM: String = "COM"

  type RWS[A] = ReaderWriterState[Unit, Int, Int, A]

  def numberOfOrbits(relationships: Map[String, List[String]]): Output = {
    List(COM).iterateUntilM[RWS] { children =>
      for {
        depth      <- ReaderWriterState.modify((previousDepth: Int) => previousDepth + 1).get
        satellites =  children.flatMap(relationships.getOrElse(_, List.empty))
        _          <- satellites.traverse[RWS, Unit](_ => ReaderWriterState.tell(depth))
      } yield satellites
    }(_.isEmpty).runEmptyL(()).value
  }

  override lazy val part1: Option[Int] = Some {
    numberOfOrbits(orbitalRelationships)
  }
}
