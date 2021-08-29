package collections

import algebra.CoMonad

case class Store[S, A](f: S => A, s: S) {
  def current: A = f(s)

  def valueAt(ss: S): A = f(ss)

  def atFocus(s: S): Store[S, A] = Store(f, s)
}

object Store {
  type IntStore[A] = Store[Int, A]

  implicit val coMonad: CoMonad[IntStore] = new CoMonad[IntStore] {
    override def extract[A](fa: IntStore[A]): A = fa.f(fa.s)

    override def coFlatten[A](fa: IntStore[A]): IntStore[IntStore[A]] =
      Store[Int, Store[Int, A]](i => Store(fa.f, i), fa.s)

    override def map[A, B](fa: IntStore[A])(f: A => B): IntStore[B] =
      Store[Int, B](fa.f andThen f, fa.s)
  }

  private def safeExtract[A](i: Int)(seq: Seq[A]): A =
    if (i < 0) seq.head
    else if (i >= seq.length) seq.last
    else seq(i)

  def fromSeq[A](seq: Seq[A]): Store[Int, A] =
    Store(safeExtract(_)(seq), 0)
}