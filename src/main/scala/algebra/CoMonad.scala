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

object CoMonad {
  implicit class CoMonadOps[F[_], A](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: CoMonad[F]): F[B] = F.map(fa)(f)

    def extract(implicit F: CoMonad[F]): A = F.extract(fa)

    def coFlatten(implicit F: CoMonad[F]): F[F[A]] = F.coFlatten(fa)

    def coFlatMap[B](fa: F[A])(f: F[A] => B)(implicit F: CoMonad[F]): F[B] =
      F.coFlatMap(fa)(f)
  }
}
