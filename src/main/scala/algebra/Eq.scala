package algebra

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

final case class IsEq[A](lhs: A, rhs: A)
