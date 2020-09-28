package examples

import state.{RunnableStateTag, STArray, STRef, StateTag}

object QuickSort {
  def quicksort(ls: List[Int]): List[Int] = {
    if (ls.isEmpty) ls else StateTag.run {
      new RunnableStateTag[List[Int]] {
        override def apply[S]: StateTag[S, List[Int]] =
          for {
            arr <- STArray.fromList(ls)
            size <- arr.size
            _ <- qs(arr, 0, size - 1)
            sorted <- arr.view
          } yield sorted
      }
    }
  }

  private def partition[S](arr: STArray[S, Int], l: Int, r: Int, pivot: Int): StateTag[S, Int] = {
    for {
      value <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      jRef <- STRef[S, Int](l)
      _ <- (l until r).foldLeft(StateTag.unit[S]) { case (accST, i) =>
        for {
          _ <- accST
          j <- jRef.read
          curr <- arr.read(i)
          _ <- {
            if (curr < value)
              arr.swap(i, j).flatMap(_ => jRef.write(j + 1))
            else
              StateTag.unit[S]
          }
        } yield ()
      }
      j <- jRef.read
      _ <- arr.swap(j, r)
    } yield j
  }

  private def qs[S](arr: STArray[S, Int], l: Int, r: Int): StateTag[S, Unit] = {
    if (l > r) StateTag(()) else {
      val m = (l + r) / 2
      for {
        pi <- partition(arr, l, r, m)
        _ <- qs(arr, l, pi - 1)
        _ <- qs(arr, pi + 1, r)
      } yield ()
    }
  }

  def main(args: Array[String]): Unit = {
    val ls = (10000 to 1 by -1).toList
    println(quicksort(ls))
  }
}
