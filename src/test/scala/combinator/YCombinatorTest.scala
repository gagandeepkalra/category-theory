package combinator

import org.scalatest.{FunSpec, Matchers}

class YCombinatorTest extends FunSpec with Matchers {

  import YCombinator._

  describe("YCombinator: should verify Y[A, B](f) gives fixed point of f") {

    it("should verify YCombinator works for factorial") {

      def factorialGenerator(f: Int => Int): Int => Int =
        (x: Int) => if (x == 0) 1 else x * f(x - 1)

      val factorial = Y(factorialGenerator)

      factorial(5) shouldBe 120
    }

    it("should verify YCombinator works for fibonacci") {

      def fibonacciGenerator(f: Int => Int): Int => Int =
        (x: Int) => if (x < 2) x else f(x - 1) + f(x - 2)

      val fibonacci = Y(fibonacciGenerator)

      fibonacci(5) shouldBe 5
    }
  }
}
