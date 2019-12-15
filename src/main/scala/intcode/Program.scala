package intcode

case class Program(underlying: Array[Int]) extends AnyVal {
  def valueAt(i: Pointer): Int = underlying(i.value)

  def setValue(index: Pointer, value: Int): Program =
    Program(underlying.updated(index.value, value))

  def outputValue: Int = underlying.head
}

case class Pointer(value: Int) extends AnyVal {
  def +(i: Int): Pointer = Pointer(value + i)
}
