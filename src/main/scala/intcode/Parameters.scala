package intcode

case class InputParameter(
  // XXX this really should be marked private, but doing so makes sbt crash for now
  // see https://github.com/sbt/zinc/issues/688
  definedFrom: Either[Pointer, Cell],
) extends AnyVal {
  def value(implicit program: Program): Cell =
    definedFrom.fold(program.valueAt, identity)

  def show(implicit program: Program): String = definedFrom match {
    case Left(pointer) => s"$pointer (resolves to ${program.valueAt(pointer)})"
    case Right(cell)   => cell.toString
  }
}

object InputParameter {
  def read(program: Program, pointer: Pointer, mode: Char): InputParameter = mode match {
    case '0' => InputParameter(Left(Pointer(program.valueAt(pointer).value)))
    case '1' => InputParameter(Right(program.valueAt(pointer)))
  }
}

case class OutputParameter(
  value: Pointer
)

object OutputParameter {
  def read(program: Program, pointer: Pointer): OutputParameter = {
    OutputParameter(Pointer(program.valueAt(pointer).value))
  }
}

object Parameters {
  def readForBinaryInstruction(program: Program, opCodePointer: Pointer, modes: Seq[Char]): (InputParameter, InputParameter, OutputParameter) = {
    (
      InputParameter.read(program, opCodePointer + 1, modes(0)),
      InputParameter.read(program, opCodePointer + 2, modes(1)),
      OutputParameter.read(program, opCodePointer + 3)
    )
  }
}
