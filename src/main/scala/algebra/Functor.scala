package algebra

import scala.language.{higherKinds, implicitConversions}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {

  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

  implicit val function0Functor: Functor[Function0] = new Functor[Function0] {
    override def map[A, B](fa: () => A)(f: A => B): () => B = () => f(fa())
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  }

  implicit def compose[F[_] : Functor, G[_] : Functor]: Functor[({type H[x] = F[G[x]]})#H] = {
    type H[x] = F[G[x]]

    new Functor[H] {
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = fa.map(_.map(f))
    }
  }

  implicit class FunctorOps[F[_], A](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)
  }

}

trait FunctorLaws[F[_]] {
  val F: Functor[F]

  def mapIdentity[A](fa: F[A]): IsEq[F[A]] = IsEq(F.map(fa)(a => a), fa)

  def mapCompose[A, B, C](fa: F[A], f: A => B, g: B => C): IsEq[F[C]] =
    IsEq(F.map(F.map(fa)(f))(g), F.map(fa)(f andThen g))

  def mapAssociativity[A, B, C, D](fa: F[A], f: A => B, g: B => C, h: C => D): IsEq[F[D]] =
    IsEq(F.map(F.map(fa)(f andThen g))(h), F.map(F.map(fa)(f))(g andThen h))
}
