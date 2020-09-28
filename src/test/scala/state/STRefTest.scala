package state

import org.scalatest.{FunSuite, Matchers}

class STRefTest extends FunSuite with Matchers {

  test("testMap") {
    val xOriginal = 5
    val yOriginal = 6

    val (xNew, yNew) = StateTag.run {
      new RunnableStateTag[(Int, Int)] {
        def apply[S]: StateTag[S, (Int, Int)] =
          for {
            xRef <- STRef(xOriginal)
            yRef <- STRef(yOriginal)
            x <- xRef.read
            y <- yRef.read
            _ <- xRef.write(y)
            _ <- yRef.write(x)
            updatedX <- xRef.read
            updatedY <- yRef.read
          } yield (updatedX, updatedY)
      }
    }

    xNew shouldBe yOriginal
    yNew shouldBe xOriginal
  }

}
