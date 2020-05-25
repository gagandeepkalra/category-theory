package algebra

trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B =
    foldLeft(fa, Monoid[B].identity)((b, a) => Monoid[B].combine(b, f(a)))
}

object Foldable {
  def apply[F[_]: Foldable]: Foldable[F] = implicitly[Foldable[F]]
}
