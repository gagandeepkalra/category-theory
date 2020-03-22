package instances

import algebra.CoMonad
import collections.Zipper

object zipper {

  val zipperCoMonad: CoMonad[Zipper] = new CoMonad[Zipper] {
    override def extract[A](fa: Zipper[A]): A = fa.focus

    override def coFlatten[A](fa: Zipper[A]): Zipper[Zipper[A]] =
      Zipper(fa.duplicateLefts, fa, fa.duplicateRights)

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      Zipper(fa.left.map(f), f(fa.focus), fa.right.map(f))
  }

  implicit class ZipperOps[A](val fa: Zipper[A]) extends AnyVal {
    def extract: A = zipperCoMonad.extract(fa)

    def coFlatten: Zipper[Zipper[A]] =
      zipperCoMonad.coFlatten(fa)

    def map[B](f: A => B): Zipper[B] =
      zipperCoMonad.map(fa)(f)

    def coFlatMap[B](f: Zipper[A] => B): Zipper[B] =
      zipperCoMonad.coFlatMap(fa)(f)
  }

}
