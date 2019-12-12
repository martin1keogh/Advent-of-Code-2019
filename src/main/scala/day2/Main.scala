package day2

import cats.data.State
import cats.syntax.monad._
import common.AoCRunnable

object Main extends AoCRunnable {
  override type Output = Int
  override val dayNumber: Int = 2

  val program: Program = inputLines().next().split(",").map(_.toInt)

  type Program = Array[Int]
  type Pointer = Int
  type ProgramOutput = Int
  type NextInstruction = Int

  def transitionFunction(program: Program, index: Pointer): Program = program.slice(index, index + 4) match {
    case Array(opcode, input1Pos, input2Pos, outputPos) =>
      val input1 = program(input1Pos)
      val input2 = program(input2Pos)

      val outputValue = opcode match {
        case 1 => input1 + input2
        case 2 => input1 * input2
      }

      program.updated(outputPos, outputValue)
  }

  def setNounAndVerb(noun: Int, verb: Int)(program: Program): Program = {
    program.updated(1, noun).updated(2, verb)
  }

  def haltCondition(instruction: NextInstruction): Boolean = {
    instruction == 99
  }

  val runner: State[Program, ProgramOutput] = {
    State[(Program, Pointer), NextInstruction] { case (currentProgram, pointer) =>
      val newProgram = transitionFunction(currentProgram, pointer)
      // This +4 might change with future specs, the new pointer value
      // should probably be returned by transitionFunction
      val newPointer = pointer + 4
      ((newProgram, newPointer), newProgram(newPointer))
    }
    .iterateUntil(haltCondition)
    .contramap[Program](_ -> 0) // add the value of the starting pointer
    .transform { case ((program, _), _) =>
      program -> program.head   // change the output of the program to be program[0], not the next instruction
    }
  }

  def processFor(noun: Int, verb: Int): ProgramOutput = {
    runner
      .contramap[Program](setNounAndVerb(noun, verb))
      .runA(program)
      .value
  }

  override val part1: Option[Int] = Some {
    processFor(12, 2)
  }

  override val part2: Option[Int] = {
    Iterator
      .tabulate(99, 99) { case (noun, verb) => (noun, verb) -> processFor(noun, verb) }
      .flatten
      .collectFirst { case ((noun, verb), 19690720) => noun * 100 + verb }
  }
}
