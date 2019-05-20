class Signal[T](expr: => T) {
  def apply(): T = ???
}
object Signal {
  def apply[T](expr: => T) = new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  def update(expr: => T): Unit = ???
}
object Var {
  def apply[T](expr: => T) = new Var(expr)
}