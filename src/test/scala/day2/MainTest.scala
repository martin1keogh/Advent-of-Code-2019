package day2

import day2.Main.process
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MainTest extends AnyWordSpec with Matchers {
  "process" should {
    "for every provided exemple" should {
      val exemples = Seq(
        "1,0,0,0,99" -> "2,0,0,0,99",
        "2,3,0,3,99" -> "2,3,0,6,99",
        "2,4,4,5,99,0" -> "2,4,4,5,99,9801",
        "1,1,1,4,99,5,6,0,99" -> "30,1,1,4,2,5,6,0,99"
      ).map { case (input, output) =>
        input.split(",").map(_.toInt) -> output.split(",").map(_.toInt)
      }

      exemples.zipWithIndex.foreach { case ((input, output), index) =>
        s"return the correct output for exemple ${index + 1}" in {
          process(input) should be (output)
        }
      }
    }
  }
}
