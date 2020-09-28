package free

import free.Trampolining.{Return, Suspend, TailRec}
import org.scalatest.{FunSpec, Matchers}

class TrampoliningSpec extends FunSpec with Matchers {

  describe("TrampoliningTest") {

    it("should throw StackOverFlowError") {

      val f = (x: Int) => x
      val g: Int => Int = List.fill(10000)(f).foldLeft(f)(_ compose _)

      assertThrows[StackOverflowError] {
        g(42)
      }

    }

    it("should throw StackOverFlowError when not wrapped in Suspend") {
      val f: (Int => TailRec[Int]) = Return(_)

      val g: Int => TailRec[Int] = List.fill(10000)(f).foldLeft(f) {
        (acc: Int => TailRec[Int], elem: Int => TailRec[Int]) => x =>
          acc(x).flatMap(elem)
      }

      assertThrows[StackOverflowError] {
        g(42)
      }

    }

    it("should not throw StackOverFlowError") {
      val f: (Int => TailRec[Int]) = Return(_)

      val g = List.fill(10000)(f).foldLeft(f) {
        (acc: Int => TailRec[Int], elem: Int => TailRec[Int]) => x =>
          Suspend(() => acc(x).flatMap(elem))
      }

      g(42).run shouldBe 42

    }

  }

  describe("Map over Tree") {

    sealed trait Tree[A] {
      def label: A

      def map[B](f: A => B): Tree[B] = this match {
        case Leaf(a)           => Leaf(f(a))
        case Node(a, children) => Node(f(a), children.map(_.map(f)))
      }
    }

    case class Leaf[A](label: A) extends Tree[A]
    case class Node[A](label: A, children: List[Tree[A]]) extends Tree[A]

    def mapR[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(a)           => Leaf(f(a))
      case Node(a, children) => Node(f(a), children.map(mapR(_)(f)))
    }

    def sequence[A](ltt: List[TailRec[A]]): TailRec[List[A]] =
      ltt.reverse.foldLeft(Return(Nil): TailRec[List[A]]) { (tla, ta) =>
        ta.map(((_: A) :: (_: List[A])).curried).flatMap(tla.map)
      }

    def mapT[A, B](tree: Tree[A])(f: A => B): TailRec[Tree[B]] = tree match {
      case Leaf(a) => Return(Leaf(f(a)))
      case Node(a, children) =>
        val listTailRec: List[TailRec[Tree[B]]] =
          children.map(t => Suspend(() => mapT(t)(f)))

        val tailRecList: TailRec[List[Tree[B]]] =
          listTailRec
            .foldLeft(Return(Nil): TailRec[List[Tree[B]]]) { (acc, e) =>
              acc.flatMap(ls => e.map(t => t :: ls))
            }
            .map(_.reverse)

        tailRecList.map(Node(f(a), _))
    }

  }
}
