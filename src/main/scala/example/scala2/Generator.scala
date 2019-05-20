package example.scala2

trait Generator[+T] {

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(Generator.this.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(Generator.this.generate).generate
  }
}
