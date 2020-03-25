package free

import free.FreeMonad._
import org.scalatest.{FunSpec, Matchers}

class FreeMonadSpec extends FunSpec with Matchers {

  describe("Trampoline using free monad") {
    it("should not throw StackOverFlowError when using runTailRec") {
      val f: (Int => Trampoline[Int]) = Return(_)

      val g: Int => Trampoline[Int] = List.fill(10000)(f).foldLeft(f) {
        (acc: Int => Trampoline[Int], elem: Int => Trampoline[Int]) => x =>
          Suspend(() => acc(x).flatMap(elem))
      }

      implicit val extract: (() => Trampoline[Int]) => Trampoline[Int] = _()

      g(42).runT.apply() shouldBe 42

    }

    it("should not throw StackOverFlowError when using runStep") {
      val f: (Int => Trampoline[Int]) = Return(_)

      val g: Int => Trampoline[Int] = List.fill(10000)(f).foldLeft(f) {
        (acc: Int => Trampoline[Int], elem: Int => Trampoline[Int]) => x =>
          Suspend(() => acc(x).flatMap(elem))
      }

      g(42).runStep.apply() shouldBe 42

    }

    it("should throw StackOverFlowError when using non tail recursive run") {
      val f: (Int => Trampoline[Int]) = Return(_)

      val g: Int => Trampoline[Int] = List.fill(10000)(f).foldLeft(f) {
        (acc: Int => Trampoline[Int], elem: Int => Trampoline[Int]) => x =>
          Suspend(() => acc(x).flatMap(elem))
      }

      assertThrows[StackOverflowError] {
        g(42).run.apply()
      }

    }
  }

}
