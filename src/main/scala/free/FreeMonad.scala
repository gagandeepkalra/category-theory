package free

import algebra.{Functor, Monad}

import scala.annotation.tailrec
import scala.language.higherKinds

/*
construction that allows us to build a very simple Monad from any functor.

Something we can wrap around an arbitrary type constructor (a F[_]) to construct a monad. It allows us to separate
the structure of the computation from its interpreter, thereby allowing different interpretation depending on context.
 */
object FreeMonad {

  sealed abstract class Free[F[_], A] {

    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case FlatMap(fa, g) => FlatMap(fa, (a: Any) => g(a).flatMap(f))
      case _              => FlatMap(this, f)
    }

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))

    /*
    returns either the next step of the computation, or the result if there are no more steps.
     */
    @tailrec
    private def resume(implicit F: Functor[F]): Either[F[Free[F, A]], A] = this match {
      case Return(a)  => Right(a)
      case Suspend(s) => Left(s)
      case FlatMap(fa, f) =>
        fa match {
          case Return(a)      => f(a).resume
          case Suspend(s)     => Left(F.map(s)(_.flatMap(f)))
          case FlatMap(ga, g) => ga.flatMap(a => g(a).flatMap(f)).resume
        }
    }

    @tailrec
    final def runT(implicit M: Monad[F],
                   extract: F[Free[F, A]] => Free[F, A]): F[A] = {
      resume match {
        case Right(a)    => M.unit(a)
        case Left(fFree) => extract(fFree).runT
      }
    }

    // tail recursive only if the monad is has stack safe flatMap
    def run(implicit M: Monad[F]): F[A] = {
      resume match {
        case Right(a)    => M.unit(a)
        case Left(fFree) => M.flatMap(fFree)(_.run)
      }
    }

    /*
    if have monad with tail recursive monadic recursion
     */
    def runStep(implicit M: Monad[F]): F[A] = {
      @scala.annotation.tailrec
      def step(free: Free[F, A]): F[Either[Free[F, A], A]] = free match {
        case Return(a)  => M.unit(Right(a))
        case Suspend(s) => M.map(s)(Left(_))
        case FlatMap(fa, f) =>
          fa match {
            case Return(a)      => M.unit(Left(f(a)))
            case Suspend(s)     => M.map(s)(free => Left(free.flatMap(f)))
            case FlatMap(ga, g) => step(ga.flatMap(a => g(a).flatMap(f)))
          }
      }

      M.tailRecM(this)(step)
    }
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[Free[F, A]]) extends Free[F, A] // or go s: F[A] and lift in resume
  case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  type Trampoline[A] = Free[Function0, A]

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
      f(fa())

    override def unit[A](a: A): () => A = () => a

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => () => Either[A, B]): () => B =
      f(a)() match {
        case Left(a) => tailRecM(a)(f)
        case Right(b) =>
          () => b
      }
  }

}
