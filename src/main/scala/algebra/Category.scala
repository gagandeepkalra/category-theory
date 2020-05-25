package algebra

trait Category[F[_, _]] {

  def identity[A]: F[A, A]

  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] =
    compose(g, f)
}

object Category {

  implicit val function1ICategory: Category[Function] = new Category[Function1] {
    override def identity[A]: A => A = a => a

    override def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  }
}

trait CategoryLaws[F[_, _]] {
  val F: Category[F]

  def leftIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    IsEq(F.compose[A, A, B](fab, F.identity[A]), fab)

  def rightIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    IsEq(F.compose[A, B, B](F.identity[B], fab), fab)

  def composeAssociativity[A, B, C, D](fab: F[A, B], fbc: F[B, C], fcd: F[C, D]): IsEq[F[A, D]] =
    IsEq(F.compose(F.compose(fcd, fbc), fab), F.compose(fcd, F.compose(fbc, fab)))

}

object CategoryLaws {
  def apply[F[_, _]: Category](): CategoryLaws[F] = new CategoryLaws[F] {
    override val F: Category[F] = implicitly[Category[F]]
  }
}
