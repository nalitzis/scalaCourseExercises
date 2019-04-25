package example

object Session {

  def abs(x:Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x) /x < 0.001

  def improve(guess: Double, x: Double) =(guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

  def factTail(x : Int): Int = {
    def factAcc(x: Int, acc: Int) : Int =
      if (x == 0) acc
      else factAcc(x - 1, acc * x)
    factAcc(x, 1)
  }

  def cube (x: Int) : Int = x*x*x
  def sumCubes = sumCurry(x => x*x*x)
  def sumSquares = sumCurry(x => x*x)

  def sum(f: Int => Int, a : Int, b : Int) : Int =
    if (a > b) 0
    else f(a) + sum(f, a+1, b)

  def sumCurry(f: Int => Int) : (Int, Int) => Int = {
    def sumF(a: Int, b: Int) : Int =
      if (a > b) 0
      else f(a) + sumF(a+1, b)
    sumF
  }


  def product(f: Int => Int, a: Int, b: Int) : Int =
    if (a > b)  1
    else f(a) * product(f, a+1,b)

  def productC(f: Int => Int): (Int, Int) => Int = {
    def productF(a:Int, b: Int) : Int =
      if (a > b) 1
      else f(a) * productF(a + 1,b)
    productF
  }

  def operationC(f: Int => Int, g: (Int, Int) => Int, zero : Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else g(f(a), operationC(f, g, zero)(a+1, b))

  def productRedux(f:Int => Int)(a: Int, b: Int) : Int = operationC(f, (x,y)=> x*y, 1)(a,b)
  def sumRedux(f:Int => Int)(a: Int, b: Int) : Int = operationC(f, (x,y)=>x+y, 0)(a,b)

  def productX(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * productX(f)(a+1, b)

  def factX(x: Int) = productX(x=>x)(1,x)

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess:Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def isCloseEnough(x: Double, y: Double) = Math.abs((x-y)/x)/x < 0.0001

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

  def sqrtX(x: Double) = fixedPoint(y => (y+x/y)/2)(1.0)
  def sqrtX2(x:Double) = fixedPoint(averageDamp(y=> x/y))(1.0)
}
