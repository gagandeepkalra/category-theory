package concurrency

import java.util.concurrent.Executors

import scala.concurrent.duration.Duration
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.{Await, ExecutionContext, Future}

object ParallelMergeSort {

  def mergeSort[A: Ordering](ls: List[A])(implicit ec: ExecutionContext): Future[List[A]] = {
    val size = ls.length
    if (size <= 8192) Future.successful(ls.sorted)
    else {
      val m = size / 2
      val (l, r) = ls.splitAt(m)
      val lFuture = mergeSort(l)
      val rFuture = mergeSort(r)

      lFuture.flatMap(lSorted => rFuture.map(rSorted => merge(lSorted, rSorted)))
    }
  }

  @scala.annotation.tailrec
  def merge[A](l: List[A], r: List[A], acc: List[A] = Nil)(implicit ord: Ordering[A]): List[A] = (l, r) match {
    case (Nil, Nil) => acc.reverse
    case (Nil, h :: t) => merge(Nil, t, h :: acc)
    case (h :: t, Nil) => merge(t, Nil, h :: acc)
    case (lh :: lt, rh :: rt) => if (ord.lt(lh, rh)) merge(lt, r, lh :: acc) else merge(l, rt, rh :: acc)
  }

  def timed[A](f: => A): (A, Long) = {
    val start = System.currentTimeMillis()

    val a = f
    (a, System.currentTimeMillis() - start)
  }

  def timedF[A](f: => Future[A]): (A, Long) = {
    val start = System.currentTimeMillis()

    val a = Await.result(f, Duration.Inf)
    (a, System.currentTimeMillis() - start)
  }

  def main(args: Array[String]): Unit = {
    val in = (1000000 to 1 by -1).toList

    println(timed(in.sorted)._2)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool(16))
    println(timedF(mergeSort(in))._2)

    implicit val ecx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))
    println(timedF(mergeSort(in))._2)
  }

}
