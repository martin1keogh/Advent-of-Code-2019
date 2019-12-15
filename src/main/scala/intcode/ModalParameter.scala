package intcode

case class ModalParameter(
  value: Either[Pointer, Int],
) extends AnyVal {
  def dereferencedValue(implicit program: Program): Int =
    value.fold(program.valueAt, identity)
}

case class OutputParameter(
  value: Pointer
)

object Parameters {
  def readForBinaryInstruction(program: Program, opCodePointer: Pointer, modes: Seq[Char]): (ModalParameter, ModalParameter, OutputParameter) = {
    (
      readModal(program, opCodePointer + 1, modes(0)),
      readModal(program, opCodePointer + 2, modes(1)),
      OutputParameter(Pointer(program.valueAt(opCodePointer + 3)))
    )
  }

  def readModal(program: Program, pointer: Pointer, mode: Char): ModalParameter = mode match {
    case '0' => ModalParameter(Left(Pointer(program.valueAt(pointer))))
    case '1' => ModalParameter(Right(program.valueAt(pointer)))
  }
}
