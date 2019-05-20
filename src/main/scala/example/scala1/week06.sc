val s = "Hello world"
val flatMapOfS = s.flatMap(c => List('.', c))
val mapOfS = s.map(c => List('.', c))
val unzipOfMapS = mapOfS.unzip(s => Pair(s.head,s.tail.head))

val sumOfS = s.sum

val M = 4
val N = 5
val combOfM_N = (1 until M).flatMap(x => (1 to N).map(y => (x,y)))

val vector1 = Vector(0,2,1,3)
val vector2 = Vector(2,4,1,1)

val zipOfVectors = vector1.zip(vector2)
val scalarProduct = zipOfVectors.map(el => el._1 * el._2).sum

def isPrime(n: Int) = (2 until n).forall(n % _ != 0)
val n = 7
val couplesWhoseSumIsPrime = (1 until n)
  .flatMap(i => (1 until i).map(j => (i, j)))
  .filter(pair => isPrime(pair._1 + pair._2))

//for has no side effects!
val couplesWithFor = for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct(xs: Seq[Int], ys: Seq[Int]): Double =
  (for ((i,j) <- xs.zip(ys)) yield i * j).sum
scalarProduct(vector1, vector2)

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else for {
      queens <- placeQueens(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens
  placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]) : Boolean = {
  val row = queens.length
  val queensWithRow = (row -1 to 0 by -1).zip(queens)
  queensWithRow.forall {
    case (r,c) => col != c && math.abs(col - c) != row - r
  }
}


def show(queens: List[Int]) = {
  val lines = for (col <- queens.reverse)
    yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

queens(4).map(show) mkString "\n"


class Poly(val terms0 : Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0.withDefaultValue(0.0)
  //def +(other: Poly) = new Poly(terms ++ other.terms.map(adjust))

  def +(other: Poly) = new Poly(other.terms.foldLeft(terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) : Map[Int, Double] = {
    terms.updated(term._1, term._2 + terms(term._1))
  }

  def adjust(other: (Int, Double)) : (Int, Double) = {
    (other._1, terms(other._1) + other._2)
  }

  override def toString = (for {
    (exp, coeff) <- terms.toList.sorted.reverse
  } yield coeff + "x^" + exp).mkString(" + ")

}

val myMap = Map(1 -> 2.0, 3 -> 0.3, 4 -> 3.1)
val pol1 = new Poly(myMap)
val pol2 = new Poly(1 -> 7.0, 2 -> 6.0)
val pol3 = pol1 + pol2

import scala.io.Source

val in = Source.fromURL("https://lamp.epfl.ch/wp-content/uploads/2019/01/linuxwords.txt")
val words = in.getLines.toList.filter(word => word.forall(c => c.isLetter))

val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode: Map[Char, Char] = for {
  (num, abc) <- mnem
  letter <- abc
} yield letter -> num

val charCode2: Map[Char, Char] = mnem.flatMap(x => x._2.map(c => (c, x._1)))

def wordCode(word: String): String = for {
  letter <- word
} yield charCode(letter.toUpper)

def wordCode2(word:String): String = word.map(c => charCode(c.toUpper))

val wordsForNum: Map[String, Seq[String]] = words.groupBy(wordCode).withDefaultValue(Seq())

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else (for {
    split <- 1 to number.length
    word <- wordsForNum(number.take(split))
    rest <- encode(number.drop(split))
  } yield word :: rest).toSet

def encode2(number: String) : Set[List[String]] =
  if (number.isEmpty) Set(List())
  else (1 to number.length).flatMap(num => wordsForNum(number.take(num)).map(word => word :: encode2(number.drop(num)))).toSet


def translate(number: String): Set[String] = encode(number).map(_.mkString(" "))

translate("7225247386")