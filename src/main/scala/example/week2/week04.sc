abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat) : Nat
  def - (that: Nat) : Nat
}

object Zero extends Nat {
  override def isZero = true

  override def predecessor = throw new IllegalStateException("zero does not have a predecessor")

  override def +(that: Nat) = that

  override def -(that: Nat) = if (that.isZero) this else throw new IllegalStateException("can' t subtract from zero")
}

class Succ(n: Nat) extends Nat {
  override def isZero = false

  override def predecessor = n

  override def +(that: Nat) = new Succ(n + that)

  override def -(that: Nat) = if (that.isZero) this else n - that.predecessor
}

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show: String = this match {
    case Number(n) => "" + n
    case Sum(e1, e2) => "(" + e1.show + " + " + e2.show + ")"
    case Prod(e1, e2) => e1.show + " * " + e2.show
  }
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

val myExpr = Sum(Sum(Number(1), Number(2)), Number(6))
myExpr.show
myExpr.eval

val myExprProd = Prod(Sum(Number(2), Number(3)), Number(5))
myExprProd.show
myExprProd.eval

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]) : List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y)  x :: y :: ys else y :: insert(x, ys)
}

val unordered : List[Int] = 7 :: 5 :: 3 :: 9 :: Nil
isort(unordered)
