package day14

import cats.{ Eval, Monoid }
import cats.data.{ IndexedStateT, State, NonEmptyList => NEL }
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.invariant._
import common.AoCRunnable

object Day14 extends AoCRunnable {
  override type Output = QuantifiedOre
  override val dayNumber: Int = 14

  final val FUEL: "FUEL" = "FUEL"
  final val ORE: "ORE" = "ORE"

  type Chemical = String
  type Quantity = Int
  type QuantifiedOre = ("ORE", Quantity)
  type QuantifiedChemical = (Chemical, Quantity)
  type RequiredInputs = NEL[QuantifiedChemical]
  type Reaction = (RequiredInputs,  QuantifiedChemical)

  val reactions: List[Reaction] = inputLines().map(parseReaction).toList

  def oreConversionHandler(reactions: List[Reaction]): IndexedStateT[Eval, Chemical, RequiredInputs, QuantifiedOre] = {
    val loop: State[RequiredInputs, Option[QuantifiedOre]] = State { inputs =>
      val (oreInputs, otherInputs) = separateOreAndOthers(inputs)

      otherInputs match {
        case _ if otherInputs.forall(_._2 <= 0) =>
          inputs -> Some(oreInputs.combineAll)

        case (chem, qtyRequired)::tl =>
          val (subInputs, (_, qtyPerReaction)) = reactions.find(_._2._1 == chem).get   // XXX unsafe, 'recipe not found' should be handled here
          val reactionsNeeded = math.ceil(qtyRequired.toDouble / qtyPerReaction).toInt // why does .ceil return a double not an int/long
          val adjustedSubInputs = subInputs.map { case (c, q) => c -> q * reactionsNeeded }
          val overProduction = List(chem -> (qtyRequired - qtyPerReaction * reactionsNeeded))
          simplifyRequiredInputs(adjustedSubInputs ++ tl ++ oreInputs ++ overProduction) -> None
      }
    }

    // only available importing cats.syntax.all._ ?
    object flatMapOptionSyntax extends cats.syntax.FlatMapOptionSyntax
    import flatMapOptionSyntax._

    loop
      .untilDefinedM
      .contramap[Chemical](chem => reactions.find(_._2._1 == chem).get._1)
  }

  // partition AND specify at type-level that the left-side of the resulting tuple contains only "ORE",
  // allowing use to use `QuantifiedOreMonoid.combineAll` without resorting to a cast
  private def separateOreAndOthers(inputs: RequiredInputs): (List[QuantifiedOre], List[QuantifiedChemical]) = {
    inputs.toList.partitionMap {
      case (ORE, qty) => Left((ORE, qty))
      case i => Right(i)
    }
  }

  private def simplifyRequiredInputs(requiredInputs: RequiredInputs): RequiredInputs = {
    NEL.fromListUnsafe(
      requiredInputs.toList
        .groupMapReduce(_._1)(_._2)(_ + _)
        .filterNot(_._2 == 0)  // not required but cleans up 'empty terms'
        .toList
        .sortBy(-_._2)
    )
  }

  def findFuelOreConversion(reactions: List[Reaction]): QuantifiedOre = {
    oreConversionHandler(reactions)
      .runA(FUEL)
      .value
  }

  override lazy val part1: Option[QuantifiedOre] = Some {
    findFuelOreConversion(reactions)
  }

  private def parseQuantifiedChemical(input: String): QuantifiedChemical = {
    val Pattern = """(\d+)\s(\w+)""".r.unanchored
    input match {
      case Pattern(quantity, chemical) => (chemical, quantity.toInt)
    }
  }

  private[day14] def parseReaction(line: String): Reaction = {
    val Array(input, output) = line.split(" => ")
    NEL.fromListUnsafe(input.split(',').map(parseQuantifiedChemical).toList) -> parseQuantifiedChemical(output)
  }

  implicit private[day14] def QuantifiedOreMonoid: Monoid[QuantifiedOre] = {
    // XXX do not replace `(ORE, _)` with `ORE -> _`, typer gets confused and types `ORE` as `String`
    cats.kernel.instances.int.catsKernelStdGroupForInt.imap[QuantifiedOre]((ORE, _))(_._2)
  }
}
