package state

import org.scalatest.{FunSuite, Matchers}

class STArrayTest extends FunSuite with Matchers {

  test("test Size") {
    val size = 5

    val computedSize: Int = StateTag.run {
      new RunnableStateTag[Int] {
        def apply[S]: StateTag[S, Int] =
          for {
            aRef <- STArray(size, ())
            size <- aRef.size
          } yield size
      }
    }

    size shouldBe computedSize
  }

  test("test Read") {
    val size = 5
    val initial = 13536

    val value: Int = StateTag.run {
      new RunnableStateTag[Int] {
        def apply[S]: StateTag[S, Int] =
          for {
            aRef <- STArray(size, initial)
            value <- aRef.read(0)
          } yield value
      }
    }

    value shouldBe initial
  }

  test("test Write") {
    val size = 5
    val initial = 13536

    val value: Int = StateTag.run {
      new RunnableStateTag[Int] {
        def apply[S]: StateTag[S, Int] =
          for {
            aRef <- STArray(size, initial)
            value <- aRef.read(0)
            _ <- aRef.write(0, value + 1)
            newValue <- aRef.read(0)
          } yield newValue
      }
    }

    value shouldBe initial + 1
  }

  test("test View") {
    val size = 5
    val initial = 13536

    val value: List[Int] = StateTag.run {
      new RunnableStateTag[List[Int]] {
        def apply[S]: StateTag[S, List[Int]] =
          for {
            aRef <- STArray(size, initial)
            value <- aRef.view
          } yield value
      }
    }

    value shouldBe List.fill(size)(initial)
  }

  test("test Fill") {
    val size = 5
    val initial = 13536

    val update: Map[Int, Int] = Map(0 -> (initial + 1), 1 -> (initial + 2))

    val (zero, one) = StateTag.run {
      new RunnableStateTag[(Int, Int)] {
        def apply[S]: StateTag[S, (Int, Int)] =
          for {
            aRef <- STArray(size, initial)
            _ <- aRef.fill(update)
            zero <- aRef.read(0)
            one <- aRef.read(1)
          } yield (zero, one)
      }
    }

    zero shouldBe initial + 1
    one shouldBe initial + 2
  }

  test("test swap") {
    val swapped = StateTag.run {
      new RunnableStateTag[List[Int]] {
        def apply[S]: StateTag[S, List[Int]] =
          for {
            aRef <- STArray.fromList(List(1,2))
            _ <- aRef.swap(0, 1)
            zero <- aRef.view
          } yield zero
      }
    }

    swapped shouldBe List(2, 1)
  }

}
