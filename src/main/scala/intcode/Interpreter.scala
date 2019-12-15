package intcode

import cats.data.State
import cats.syntax.monad._

object Interpreter {
  type ProgramOutput = Int
  type NextInstruction = Int

  private def haltCondition(instruction: NextInstruction): Boolean = {
    instruction == 99
  }

  val runner: State[Program, ProgramOutput] = {
    State[(Program, Pointer), NextInstruction] { case (currentProgram, pointer) =>
      val instruction = Instruction.readAt(currentProgram, pointer)
      val newProgram = instruction.applyTo(currentProgram)
      val newPointer = pointer + instruction.numberOfConsumedCells
      ((newProgram, newPointer), newProgram.valueAt(newPointer))
    }
    .iterateUntil(haltCondition)
    .contramap[Program](_ -> Pointer(0)) // add the value of the starting pointer
    .transform { case ((program, _), _) =>
      program -> program.outputValue   // change the output of the program to be program[0], not the next instruction
    }
  }
}
