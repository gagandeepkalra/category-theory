import composition.Composition
import org.scalatest.{FunSpec, Matchers}

class CompositionSpec extends FunSpec with Matchers with Composition {

  describe("test identity") {
    it("should work with type String") {
      identity("abc") shouldBe "abc"
    }

    it("should work with type Double") {
      identity(5.23457) shouldBe 5.23457
    }
  }

  describe("test composition") {
    it("should work with identity function") {
      compose[Int, Int, Int](identity, identity)(5) shouldBe 5
    }

    it("should be associative") {

      def charToInt(c: Char): Int = c.toInt

      def intToDouble(i: Int): Double = i.toDouble

      def doubleToString(d: Double): String = d.toString

      val f = compose(doubleToString, compose(intToDouble, charToInt))
      val g = compose(compose(doubleToString, intToDouble), charToInt)

      f('a') shouldBe g('a')
      f('b') shouldBe g('b')
    }
  }
}
