

def sieve(s : Stream[Int]) : Stream[Int] = s.head #:: sieve(s.tail.filter(_ % s.head != 0))

def from(n: Int) : Stream[Int] = n #:: from(n + 1)

val nats = from(0)

val first30Numbers = nats.take(30).toList

val first30Primes = 1 :: sieve(from(2)).take(29).toList

def firstNPrimes(n : Int) = 1 :: sieve(from(2)).take(n - 1).toList

firstNPrimes(7)

def sqrtStream(x: Double) : Stream[Double] = {
  def improve(guess: Double) = {
    (guess + x / guess) / 2
  }
  lazy val guesses : Stream[Double] = 1 #:: guesses.map(x => improve(x))
  guesses
}

sqrtStream(2.0).take(10).toList