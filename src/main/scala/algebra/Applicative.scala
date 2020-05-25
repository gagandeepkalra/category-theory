package algebra

import scala.language.implicitConversions


trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fp: A => B => C = f.curried

    ap(ap(pure((a: A) => fp(a)))(fa))(fb)
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fp: A => B => C => D = f.curried

    ap(ap(ap(pure((a: A) => fp(a)))(fa))(fb))(fc)
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

}

object Applicative {
  def apply[F[_] : Applicative]: Applicative[F] = implicitly[Applicative[F]]

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def ap[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = for (f <- fab; a <- fa) yield f(a)
  }

  implicit def compose[F[_] : Applicative, G[_] : Applicative]: Applicative[({type H[x] = F[G[x]]})#H] = {
    type H[x] = F[G[x]]

    new Applicative[H] {
      override def pure[A](a: A): F[G[A]] = Applicative[F].pure(Applicative[G].pure(a))

      override def ap[A, B](fgf: F[G[A => B]])(fga: F[G[A]]): F[G[B]] = {
        val F = Applicative[F]
        val G = Applicative[G]

        F.ap(F.map(fgf)((gf: G[A => B]) => (ga: G[A]) => G.ap(gf)(ga)))(fga)
      }
    }
  }

  implicit def pure[F[_] : Applicative, A](a: A): F[A] = Applicative[F].pure(a)

  implicit class MonadOps[F[_], A](val fa: F[A]) extends AnyVal {
    def ap[B](fab: F[A => B])(implicit F: Applicative[F]): F[B] = F.ap(fab)(fa)
  }

}

trait ApplicativeLaws[F[_]] extends FunctorLaws[F] {
  val F: Applicative[F]

  import F._

  def applicativeIdentity[A](fa: F[A]): IsEq[F[A]] =
    IsEq(ap[A, A](pure((a: A) => a))(fa), fa)

  /* String length and add */
  def applicativeHomomorphism[A, B](a: A, f: A => B): IsEq[F[B]] =
    IsEq(ap(pure(f))(F.pure(a)), F.pure(f(a)))

  def applicativeInterchange[A, B](a: A, ff: F[A => B]): IsEq[F[B]] =
    IsEq(ap(ff)(F.pure(a)), ap(pure((f: A => B) => f(a)))(ff))

  def applicativeMap[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    IsEq(map(fa)(f), ap(pure(f))(fa))
}

