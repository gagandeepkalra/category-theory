package collections

import algebra.Foldable

sealed trait Tree[A] {
  def label: A

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Node(a, children) => Node(f(a), children.map(_.map(f)))
  }
}

case class Leaf[A](label: A) extends Tree[A]

case class Node[A](label: A, children: List[Tree[A]]) extends Tree[A]

object Tree {
  implicit val foldable: Foldable[Tree] = new Foldable[Tree] {
    override def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B =
      fa match {
        case Leaf(label) => f(b, label)
        case Node(label, children) => children.foldLeft(f(b, label))((acc, tree) => foldLeft(tree, acc)(f))
      }
  }
}
