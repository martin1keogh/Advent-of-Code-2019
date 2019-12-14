package intcode

import cats.data.State
import cats.syntax.monad._

object Interpreter {
  type Program = Array[Int]
  type Pointer = Int
  type ProgramOutput = Int
  type NextInstruction = Int

  private def transitionFunction(program: Program, index: Pointer): Program = program.slice(index, index + 4) match {
    case Array(opcode, input1Pos, input2Pos, outputPos) =>
      val input1 = program(input1Pos)
      val input2 = program(input2Pos)

      val outputValue = opcode match {
        case 1 => input1 + input2
        case 2 => input1 * input2
      }

      program.updated(outputPos, outputValue)
  }

  private def haltCondition(instruction: NextInstruction): Boolean = {
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
}
