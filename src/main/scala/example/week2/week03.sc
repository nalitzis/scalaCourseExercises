
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(i: Int, left: IntSet, right: IntSet) extends IntSet {
  override def toString: String = "{" + left + i + right + "}"

  override def incl(x: Int) =
    if (x < i) new NonEmpty(i, left incl x, right)
    else if (x > i) new NonEmpty(i, left, right incl x)
    else this

  override def contains(x: Int) =
    if (x < i) left contains x
    else if (x > i) right contains x
    else true

  override def union(other: IntSet) =
    left.union(right).union(other).incl(i)
}

object Empty extends IntSet {

  override def toString: String = "."

  override def incl(x: Int) = new NonEmpty(x, Empty, Empty)

  override def contains(x: Int) = false

  override def union(other: IntSet) = other
}

val ex0 = new NonEmpty(4, Empty, Empty)
val ex1 = new NonEmpty(1, Empty, Empty)
val ex2 = new NonEmpty(3, ex1, ex0)
Empty.union(ex0)
ex1.union(ex0)

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

object TestList {
  def nth[T](n: Int, list: List[T]) : T = {
     if (list.isEmpty) throw new IndexOutOfBoundsException("index is out of range")
     else if (n == 0) list.head
     else nth(n - 1, list.tail)
  }
}

val myList = new Cons[Int](2, new Cons(3, Nil()))
myList.isEmpty
TestList.nth(0, myList)
TestList.nth(1, myList)
TestList.nth(2, myList)

object List {
  def apply[T] = Nil
  def apply[T](x1: T) = new Cons[T](x1, Nil)
  def apply[T](x1: T, x2: T) = new Cons[T](x1, new Cons[T](x2, Nil))
}

val mylist = List[Int](1, 3)
val mmm = List()