package algebra

import scala.language.higherKinds

/**
  * CoMonads are context dependent computations, dual of Monad.
  */
trait CoMonad[F[_]] extends Functor[F] {
  def extract[A](fa: F[A]): A

  def coFlatten[A](fa: F[A]): F[F[A]]

  def coFlatMap[A, B](fa: F[A])(f: F[A] => B): F[B] = map(coFlatten(fa))(f)
}
