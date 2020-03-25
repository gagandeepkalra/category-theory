package instances

import algebra.CoMonad
import collections.{GridZipper, Zipper}
import instances.zipper._

object gridZipper {

  val gridZipperCoMonad: CoMonad[GridZipper] = new CoMonad[GridZipper] {
    override def extract[A](fa: GridZipper[A]): A = fa.value.focus.focus

    override def coFlatten[A](fa: GridZipper[A]): GridZipper[GridZipper[A]] = {
      GridZipper(nest(nest(fa.value)).map(_.map(GridZipper(_))))
    }

    private def nest[A](s: Zipper[Zipper[A]]): Zipper[Zipper[Zipper[A]]] = {
      val duplicateLefts: Stream[Zipper[Zipper[A]]] = {
        Stream.iterate(s)(current => current.map(_.moveLeft))
          .tail
          .zip(s.left)
          .map(_._1)
      }

      val duplicateRights: Stream[Zipper[Zipper[A]]] =
        Stream.iterate(s)(current => current.map(_.moveRight))
          .tail
          .zip(s.right)
          .map(_._1)

      Zipper(duplicateLefts, s, duplicateRights)
    }

    override def map[A, B](fa: GridZipper[A])(f: A => B): GridZipper[B] = {
      GridZipper(fa.value.map(_.map(f)))
    }
  }

  implicit class GridZipperOps[A](val fa: GridZipper[A]) extends AnyVal {
    def extract: A = gridZipperCoMonad.extract(fa)

    def coFlatten: GridZipper[GridZipper[A]] =
      gridZipperCoMonad.coFlatten(fa)

    def map[B](f: A => B): GridZipper[B] =
      gridZipperCoMonad.map(fa)(f)

    def coFlatMap[B](f: GridZipper[A] => B): GridZipper[B] =
      gridZipperCoMonad.coFlatMap(fa)(f)
  }

}
