class Chapter2 {

  def fib(n: Int) : Int = {
    @annotation.tailrec
    def fibAcc(n: Int, prev: Int, acc: Int): Int = {
      if (n <= 0) acc
      else {
        println("fibAcc(" + (n -1) + ", " + (prev + acc) + ", " + prev + ")")
        fibAcc(n - 1, prev = prev + acc, acc = prev)
      }
    }
    fibAcc(n, 1, 0)
  }

  def findFirst[A](as: Array[A], p: A=> Boolean) : Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n-2), as(n-1))) loop(n + 1)
      else false
    loop(2)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => b: B => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

}
val ch2 = new Chapter2()
println(ch2.fib(12))
println(ch2.findFirst(Array(1,2,3,4,5,6,7), (x: Int) => x == 1))
println(ch2.isSorted(Array(1,2,3,4,6,5,7), (x: Int, y: Int) => x < y))
println(ch2.isSorted(Array(1,2,3,4,5,6,7), (x: Int, y: Int) => x < y))
println(ch2.isSorted(Array(1,2,3,4,6,6,7), (x: Int, y: Int) => x <= y))
println(ch2.isSorted(Array(1,2), (x: Int, y: Int) => x <= y))
println(ch2.isSorted(Array(1), (x: Int, y: Int) => x <= y))

val x = ch2.curry((a: Int, b: Int) => a+b)
println(x(2)(3))

