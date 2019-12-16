package intcode

import cats.Eval
import cats.data.{ IndexedStateT, State }
import cats.syntax.functor._
import cats.syntax.monad._

trait Interpreter {
  private type NextInstruction = Cell

  private def haltCondition(instruction: NextInstruction): Boolean = {
    instruction.value == 99
  }

  val runner: IndexedStateT[Eval, Program, (Program, Pointer), Unit] = {
    State[(Program, Pointer), NextInstruction] { case (currentProgram, pointer) =>
      val instruction = Instruction.readAt(currentProgram, pointer)
      val newProgram = instruction.applyTo(currentProgram)
      val newPointer = pointer + instruction.numberOfConsumedCells
      ((newProgram, newPointer), newProgram.valueAt(newPointer))
    }
    .iterateUntil(haltCondition)
    .contramap[Program](_ -> Pointer(0)) // add the value of the starting pointer
    .as(())
  }
}
