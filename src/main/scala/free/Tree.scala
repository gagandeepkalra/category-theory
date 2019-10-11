package free

import free.Trampolining.{Return, Suspend, TailRec}
import free.Tree.{Leaf, Node}

sealed trait Tree[A] {
  def label: A

  def mapR[B](f: A => B): Tree[B] = this match {
    case Leaf(a)           => Leaf(f(a))
    case Node(a, children) => Node(f(a), children.map(_.mapR(f)))
  }

  def mapT[B](f: A => B): TailRec[Tree[B]] = this match {
    case Leaf(a) => Return(Leaf(f(a)))
    case Node(a, children) =>
      val listTailRec: List[TailRec[Tree[B]]] =
        children.map(c => Suspend(() => c.mapT(f)))
      val tailRecList: TailRec[List[Tree[B]]] =
        listTailRec.reverse
          .foldLeft(Return(Nil): TailRec[List[Tree[B]]]) { (acc, e) =>
            acc.flatMap(ls => e.map(t => t :: ls))
          } // or use sequence

      tailRecList.map(Node(f(a), _))
  }
}

object Tree {
  case class Leaf[A](label: A) extends Tree[A]

  case class Node[A](label: A, children: List[Tree[A]]) extends Tree[A]

  def sequence[A](ltt: List[TailRec[A]]): TailRec[List[A]] =
    ltt.reverse.foldLeft(Return(Nil): TailRec[List[A]]) { (tla, ta) =>
      ta.map(((_: A) :: (_: List[A])).curried).flatMap(tla.map)
    }
}
