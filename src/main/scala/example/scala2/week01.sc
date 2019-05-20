import example.scala2.{Generator, Inner, Leaf, Tree}

import scala.util.Try

def integers: Generator[Int] = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

def booleans = new Generator[Boolean] {
  override def generate: Boolean = integers.generate > 0
}

def booleansFor = for (x <- integers) yield x > 0

val i = integers.generate
val b = booleans.generate
val b2 = booleansFor.generate

def pairs[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T,U)] {
  override def generate: (T, U) = (t.generate, u.generate)
}

def pairsFor[T,U](t: Generator[T], u: Generator[U]) = for {
  x <- t
  y <- u
} yield (x,y)

val p1 = pairs(integers, integers).generate
val p2 = pairsFor(integers, integers).generate

val pb = pairsFor(booleans, booleans).generate

def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] = for {
  x <- integers
} yield lo + x % (hi - lo)

val between = choose(2, 12).generate

def oneOf[T](xs : T*): Generator[T] = for {
  i <- choose(0, xs.length)
} yield xs(i)

//val randomFromList = oneOf(1,2,3,4,5,6,7,8).generate

def lists: Generator[List[Int]] = for {
  //here we decide if our generated list should be empty or non empty.
  isEmpty <- booleans
  //them, based on that, we call the function to generate a (non) empty list
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)

def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

val randomList = lists.generate

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if(isLeaf) leafGenerator else innerNodeGenerator
} yield tree

def leafGenerator = for {
  x <- integers
} yield Leaf(x)

def innerNodeGenerator = for {
  x <- trees
  y <- trees
} yield Inner(x,y)

val genTree = trees.generate

val t = Try(3)
