package state

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec


/**
 * State Monad with protected run method
 *
 * Two static guarantees for purely functional mutation
 *
 * 1. If we hold reference to a mutable object, then nothing can observe us mutating it.
 * 2. A mutable object can never be observed outside of the scope in which it was created.
 *
 * `run` method is protected because an `S` represents an ability to mutate state and we don't want the mutation to escape.
 */
sealed trait StateTag[S, A] {
  self =>

  protected def run(s: S): TailRec[(A, S)]

  def map[B](f: A => B): StateTag[S, B] = new StateTag[S, B] {
    override protected def run(s: S): TailRec[(B, S)] = self.run(s).map { case (a, s) => (f(a), s) }
  }

  def flatMap[B](f: A => StateTag[S, B]): StateTag[S, B] = new StateTag[S, B] {
    override protected def run(s: S): TailRec[(B, S)] = {
      TailCalls.tailcall(self.run(s)).flatMap { case (a, s) => f(a).run(s) }
    }
  }

}

object StateTag {
  def apply[S, A](a: => A): StateTag[S, A] = new StateTag[S, A]() {
    lazy val memo: A = a

    override protected def run(s: S): TailRec[(A, S)] = TailCalls.done((memo, s))
  }

  def unit[S]: StateTag[S, Unit] = StateTag(())

  def run[A](runnable: RunnableStateTag[A]): A = {
    runnable[Unit].run(()).result._1
  }
}

/**
 * apply binds S to StateTag[S, A], function from type S to StateTag
 */
trait RunnableStateTag[A] {
  def apply[S]: StateTag[S, A]
}
