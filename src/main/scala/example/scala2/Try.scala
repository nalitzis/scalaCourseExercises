package example.scala2

import scala.util.control.NonFatal

abstract class Try[+T]
case class Success[T](x: T) extends Try[T]
case class  Failure(ex: Exception) extends Try[Nothing]

/*
object Try {
  def apply: Try[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(ex) => Failure(ex)
    }
}*/