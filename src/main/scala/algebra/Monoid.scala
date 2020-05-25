package algebra

trait Monoid[A] {
  def identity: A

  def combine(x: A, y: A): A
}

object Monoid {

  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def identity: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit def optionMonoid[A](implicit M: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def identity: Option[A] = None

      def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
        case (x, None) => x
        case (None, y) => y
        case (Some(x), Some(y)) => Some(M.combine(x, y))
      }
    }

  implicit def functionMonoid[A, B](implicit M: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def identity: A => B = _ => M.identity

    def combine(x: A => B, y: A => B): A => B = { a =>
      M.combine(x(a), y(a))
    }
  }

  implicit def productMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
    override def identity: (A, B) = (Monoid[A].identity, Monoid[B].identity)

    override def combine(x: (A, B), y: (A, B)): (A, B) = (Monoid[A].combine(x._1, y._1), Monoid[B].combine(x._2, y._2))
  }

  implicit class MonoidOps[A](val a: A) extends AnyVal {
    def combine(b: A)(implicit F: Monoid[A]): A = F.combine(a, b)
  }

}

trait MonoidLaws[A] {
  val F: Monoid[A]

  def leftIdentity(a: A): IsEq[A] = IsEq(F.combine(F.identity, a), a)

  def rightIdentity(a: A): IsEq[A] = IsEq(F.combine(a, F.identity), a)

  def combineAssociativity(x: A, y: A, z: A): IsEq[A] =
    IsEq(F.combine(F.combine(x, y), z), F.combine(x, F.combine(y, z)))

}
