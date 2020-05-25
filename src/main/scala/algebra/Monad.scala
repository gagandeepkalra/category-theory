package algebra

import scala.language.{higherKinds, implicitConversions}

/**
 * Monads are context producing computations
 */
trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  override def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] = flatMap(fab)(f => map(fa)(f))

  /*
  any operation that you would write using recursive flatMap can be rewritten to use tailRecM
   */
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = flatMap(f(a)) {
    case Left(a) => tailRecM(a)(f)
    case Right(b) => pure(b)
  }
}

object Monad {
  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  implicit def pure[F[_] : Monad, A](a: A): F[A] = Monad[F].pure(a)

  implicit class MonadOps[F[_], A](val fa: F[A]) extends AnyVal {
    def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)
  }

}

trait MonadLaws[F[_]] {
  implicit val F: Monad[F]

  import Monad._

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEq[F[C]] =
    IsEq(fa.flatMap(f).flatMap(g), fa.flatMap(a => f(a).flatMap(g)))
}