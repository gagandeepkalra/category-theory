package state

import org.scalatest.{FunSuite, Matchers}

class StateTagTest extends FunSuite with Matchers {

  test("test Unit") {
    val ex: Int = StateTag.run {
      new RunnableStateTag[Int] {
        def apply[S]: StateTag[S, Int] =
          for {
            _ <- StateTag.unit[S]
            s <- StateTag(5)
          } yield s
      }
    }

    ex shouldBe 5
  }

}
