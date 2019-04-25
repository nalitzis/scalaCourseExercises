def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(_) => List()
  case y :: ys => y :: init(ys)
}

val l = List(1, 2, 4, 5, 7)
println(init(l))

def removeAt[T](n: Int, xs: List[T]) = xs.take(n) ::: xs.drop(n+1)

println(removeAt(2, l))

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (z :: zs) :: ys => flatten(z :: zs) ::: flatten(ys)
  case  y :: ys => y :: flatten(ys)
}

val l2 = List(List(1,1), 2, 4, List(List(6,7),8))
println(flatten(l2))
println(flatten(List()))
println(flatten(Nil))
println(flatten(List(4,5,List(4,33))))

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x::xs1, y::ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, snd) = xs.splitAt(n)
    merge(msort(fst), msort(snd))
  }
}

val l3 = List(9, 4, 1, 3, 5, 2, 8, 11, 6)
println(msort(l3))

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList(ys)
}
println(squareList(l3))

def squareListMap(xs: List[Int]): List[Int] = xs map (x => x*x)
println(squareListMap(l3))

def letters = List("a","a","a","b","c","c","a")

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => (x :: xs1.takeWhile(x => sameAsListHead(x, xs))) :: pack((x :: xs1).dropWhile(x => sameAsListHead(x, xs)))
}

def sameAsListHead[T](elem: T, list: List[T]): Boolean = list match {
  case Nil => false
  case x :: _ => elem == x
}

println(pack(letters))

def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(x => (x.head, x.length))
println(encode(letters))

def mapFun[T, U](xs: List[T], f: T => U): List[U] = (xs foldRight List[U]()) ((y, ys) => f(y) :: ys)

def words : List[String] = List("asd", "a", "pasdasc", "aw")
println(mapFun(words, (a : String) => a.length))

def lengthFun[T](xs: List[T]): Int = (xs foldRight 0)((_, el) => el + 1)
println(lengthFun(words))