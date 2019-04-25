


class Rational(x: Int, y: Int) {
  require(y != 0, "denominator is 0")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
  private def g = gcd(x,y)
  val numer = x/g
  val denom = y/g

  def neg = new Rational(numer * -1, denom)

  def +(that:Rational) = operation(that, (x,y) => x+y)
  def subX(that:Rational) = operation(that, (x,y)=> x-y)

  def operation(that: Rational, g: (Int, Int) => Int) : Rational = {
    new Rational(g(numer * that.denom, that.numer*denom), denom*that.denom)
  }

  def < (that: Rational) = numer * that.denom < that.numer * denom
  def max(that: Rational) = if(this < (that)) that else this
  override def toString = numer + "/" + denom
}

val v = new Rational(2,3)
val s = new Rational(7,5)
println("numer: " + v.numer)
println("neg:" + v.neg)

println(v + s)
println(v < (s))

println(new Rational(2))