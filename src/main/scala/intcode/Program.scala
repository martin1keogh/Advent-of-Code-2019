package intcode

case class Program(
  underlying: Array[Cell],
  lastDiagnosticCode: Option[Cell] = None
) {
  def valueAt(pointer: Pointer): Cell = underlying(pointer.value)

  def setValueAt(pointer: Pointer, cell: Cell): Program =
    copy(underlying = underlying.updated(pointer.value, cell))
}

case class Pointer(value: Int) extends AnyVal {
  def +(i: Int): Pointer = Pointer(value + i)
}

case class Cell(value: Int) extends AnyVal
