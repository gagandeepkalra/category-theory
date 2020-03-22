package instances

import algebra.CoMonad
import collections.Zipper

trait ZipperInstances {
  implicit val zipperCoMonad: CoMonad[Zipper] = new CoMonad[Zipper] {
    override def extract[A](fa: Zipper[A]): A = fa.focus

    override def coFlatten[A](fa: Zipper[A]): Zipper[Zipper[A]] =
      Zipper(fa.duplicateLefts, fa, fa.duplicateRights)

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      Zipper(fa.left.map(f), f(fa.focus), fa.right.map(f))
  }
}

object ZipperInstances extends ZipperInstances
