import Trampolining.{Return, Suspend, TailRec}
import org.scalatest.{FunSpec, Matchers}

import scala.util.control.TailCalls

class TrampoliningSpec extends FunSpec with Matchers {

  describe("TrampoliningTest") {

    it("should throw StackOverFlowError") {

      val f = (x: Int) => x
      val g: Int => Int = List.fill(10000)(f).foldLeft(f)(_ compose _)

      assertThrows[StackOverflowError] {
        g(42)
      }

    }

    TailCalls

    it("should throw StackOverFlowError when not wrapped in Suspend") {
      val f: (Int => TailRec[Int]) = Return(_)

      val g: Int => TailRec[Int] = List.fill(10000)(f).foldLeft(f) {
        (acc: Int => TailRec[Int], elem: Int => TailRec[Int]) =>
          x => acc(x).flatMap(elem)
      }

      assertThrows[StackOverflowError] {
        g(42)
      }

    }

    it("should not throw StackOverFlowError") {
      val f: (Int => TailRec[Int]) = Return(_)

      val g = List.fill(10000)(f).foldLeft(f) {
        (acc: Int => TailRec[Int], elem: Int => TailRec[Int]) =>
          x => Suspend(() => acc(x).flatMap(elem))
      }

      Trampolining.run(g(42)) shouldBe 42

    }

  }
}
