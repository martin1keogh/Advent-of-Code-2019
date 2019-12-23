package day22

import common.AoCRunnable

object Day22 extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 22

  type Deck = Vector[Int]
  private val numberOfCards = 10007
  val sortedDeck: Vector[Int] = Range(0, numberOfCards).toVector

  type ShufflingTechnique = Deck => Deck

  val dealIntoNewStack: ShufflingTechnique = _.reverse

  def cut(n: Int): ShufflingTechnique = { deck =>
    if (n >= 0) {
      val (left, right) = deck.splitAt(n)
      right ++ left
    }
    else {
      val absN = math.abs(n)
      val (left, right) = (deck.dropRight(absN), deck.takeRight(absN)) // no .splitAtRight unfortunately
      right ++ left
    }
  }

  def dealWithIncrement(n: Int): ShufflingTechnique = { deck =>
    val array = Array.ofDim[Int](deck.size)
    for (i <- deck.indices) {
      array.update((i * n) % deck.size, deck(i))
    }
    array.toVector
  }

  def parseLine(line: String): ShufflingTechnique = {
    val DealNewStackRegex = "deal into new stack".r
    val CutRegex = """cut (-?\d+)""".r
    val DealIncrementRegex = """deal with increment (-?\d+)""".r
    line match {
      case DealNewStackRegex() => dealIntoNewStack
      case CutRegex(n) => cut(n.toInt)
      case DealIncrementRegex(n) => dealWithIncrement(n.toInt)
    }
  }

  lazy val techniques: List[ShufflingTechnique] = inputLines().map(parseLine).toList

  def applyTechniques(inputDeck: Deck, techniques: List[ShufflingTechnique]): Deck = Function.chain(techniques)(inputDeck)

  override lazy val part1: Option[Int] = Some {
    applyTechniques(sortedDeck, techniques).indexWhere(_ == 2019)
  }
}
