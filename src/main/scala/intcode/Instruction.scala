package intcode

sealed trait Instruction extends Product {
  def applyTo(implicit program: Program): Program
  def numberOfConsumedCells: Int = productArity + 1
}

object Instruction {
  def readAt(program: Program, pointer: Pointer): Instruction = {
    val padded = "%05d".format(program.valueAt(pointer).value)
    val (reversedModes, opCode) = padded.splitAt(3)
    val modes = reversedModes.reverse

    opCode match {
      case "01" => Addition.tupled(Parameters.readForBinaryInstruction(program, pointer, modes))
      case "02" => Multiplication.tupled(Parameters.readForBinaryInstruction(program, pointer, modes))
      case "03" => Read(OutputParameter.read(program, pointer + 1))
      case "04" => Write(InputParameter.read(program, pointer + 1, modes.head), program.valueAt(pointer + 2))
    }
  }
}

sealed trait BinaryInstruction extends Instruction {
  def lhs: InputParameter
  def rhs: InputParameter
  def binaryOp(lhs: InputParameter, rhs: InputParameter)(implicit program: Program): Cell

  def output: OutputParameter

  override def applyTo(implicit program: Program): Program = {
    program.setValueAt(output.value, binaryOp(lhs, rhs)(program))
  }
}

case class Addition(
  lhs: InputParameter,
  rhs: InputParameter,
  output: OutputParameter
) extends BinaryInstruction {
  override def binaryOp(lhs: InputParameter, rhs: InputParameter)(implicit program: Program): Cell = {
    Cell(lhs.value.value + rhs.value.value)
  }
}

case class Multiplication(
  lhs: InputParameter,
  rhs: InputParameter,
  output: OutputParameter
) extends BinaryInstruction {
  override def binaryOp(lhs: InputParameter, rhs: InputParameter)(implicit program: Program): Cell = {
    Cell(lhs.value.value * rhs.value.value)
  }
}

case class Read(
  output: OutputParameter
) extends Instruction {
  override def applyTo(implicit program: Program): Program = {
    // no real definition of what 'reads input' means yet (or how multiple (intertwined?)
    // input should be handled...) so let's just say it's one for now
    def readValue = 1
    program.setValueAt(output.value, Cell(readValue))
  }
}

case class Write(
  input: InputParameter,
  nextInstruction: Cell
) extends Instruction {
  override def numberOfConsumedCells: Int = 2

  override def applyTo(implicit program: Program): Program = {
    // no specs yet, except that non-zero values means something went wrong
    assert(input.value.value == 0 || nextInstruction.value == 99, s"read non-zero value of ${input.show}")
    program.copy(lastDiagnosticCode = Some(input.value))
  }
}
