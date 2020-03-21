package composition

trait Composition {

  def compose[A, B, C](g: B => C, f: A => B): A => C = {
    a => g(f(a))
  }

  def identity[A](a: A): A = a
}
