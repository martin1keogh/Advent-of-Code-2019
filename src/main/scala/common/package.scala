package object common {
  implicit class ChainOps[A](val value: A) extends AnyVal {
    def |>[B](f: A => B): B = f(value)
  }
}
