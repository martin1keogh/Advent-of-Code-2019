package intcode

sealed trait Instruction {
  def applyTo(implicit program: Program): Program
}

object Instruction {
  def readAt(program: Program, pointer: Pointer): Instruction = {
    val padded = "%05d".format(program.valueAt(pointer))
    val (reversedModes, opCode) = padded.splitAt(3)
    val modes = reversedModes.reverse

    opCode match {
      case "01" => Addition.tupled(Parameters.readForBinaryInstruction(program, pointer, modes))
      case "02" => Multiplication.tupled(Parameters.readForBinaryInstruction(program, pointer, modes))
    }
  }
}

sealed trait BinaryInstruction extends Instruction {
  def lhs: ModalParameter
  def rhs: ModalParameter
  def binaryOp(lhs: ModalParameter, rhs: ModalParameter)(implicit program: Program): Int

  def output: OutputParameter

  override def applyTo(implicit program: Program): Program = {
    program.setValue(output.value, binaryOp(lhs, rhs)(program))
  }
}

case class Addition(
  lhs: ModalParameter,
  rhs: ModalParameter,
  output: OutputParameter
) extends BinaryInstruction {
  override def binaryOp(lhs: ModalParameter, rhs: ModalParameter)(implicit program: Program): Int = {
    lhs.dereferencedValue + rhs.dereferencedValue
  }
}

case class Multiplication(
  lhs: ModalParameter,
  rhs: ModalParameter,
  output: OutputParameter
) extends BinaryInstruction {
  override def binaryOp(lhs: ModalParameter, rhs: ModalParameter)(implicit program: Program): Int = {
    lhs.dereferencedValue * rhs.dereferencedValue
  }
}
