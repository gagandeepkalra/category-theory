package algebra

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  /*
  any operation that you would write using recursive flatMap can be rewritten to use tailRecM
   */
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = flatMap(f(a)) {
    case Left(a)  => tailRecM(a)(f)
    case Right(b) => unit(b)
  }
}