package intcode

sealed trait Instruction extends Product {
  def applyTo(implicit program: Program): Program
  def numberOfConsumedCells: Int = productArity + 1
}

object Instruction {
  def readAt(program: Program, pointer: Pointer): Instruction = {
    val padded = "%05d".format(program.valueAt(pointer))
    val (reversedModes, opCode) = padded.splitAt(3)
    val modes = reversedModes.reverse

    opCode match {
      case "01" => Addition.tupled(Parameters.readForBinaryInstruction(program, pointer, modes))
      case "02" => Multiplication.tupled(Parameters.readForBinaryInstruction(program, pointer, modes))
      case "03" => Read(OutputParameter(pointer + 1))
      case "04" => Write(Parameters.readModal(program, pointer + 1, modes.head))
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

case class Read(
  output: OutputParameter
) extends Instruction {
  override def applyTo(implicit program: Program): Program = {
    // no real definition of what 'reads input' means yet (or how multiple (intertwined?)
    // input should be handled... so let's just say it's one for now
    def readValue = 1
    program.setValue(output.value, readValue)
  }
}

case class Write(
  input: ModalParameter
) extends Instruction {
  override def applyTo(implicit program: Program): Program = {
    // no specs yet, except that non-zero values means something went wrong
    assert(!(input.dereferencedValue == 0))
    program
  }
}
