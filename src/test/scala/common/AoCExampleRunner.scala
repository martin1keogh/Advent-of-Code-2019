package common

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait AoCExampleRunner extends AnyWordSpec with Matchers {
  type Input
  type Output

  val examples: Map[Input, Output]
  def method: Input => Output

  def examplesSolver(): Unit = {
    examples.zipWithIndex.foreach { case ((input, output), index) =>
      s"return the correct output for example ${index + 1}" in {
        method(input) should be(output)
      }
    }
  }
}
