package algebra

import scala.language.higherKinds

/**
  * Monads are context producing computations
  */
trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  /*
  any operation that you would write using recursive flatMap can be rewritten to use tailRecM
   */
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = flatMap(f(a)) {
    case Left(a)  => tailRecM(a)(f)
    case Right(b) => unit(b)
  }
}
