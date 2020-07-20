package concurrency

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success, Try}

object Course {

  /*
  Concurrency-

    Concurrency is when more than one task can start and complete in overlapping time periods. It doesn’t matter whether
  they’re running at the same instant. You can write concurrent programs on a single CPU (single execution core) machine
  where only one task can execute at a given point of time. Typically multiple tasks are executed in a time-slice manner,
  where a scheduler (such as the JVM) will guarantee each process a regular “slice” of operating time. This gives the
  illusion of parallelism to the users. And the common de facto standard way to implement a multitasking application is
  to use threads.

  A program structuring technique in which there are multiple logical threads of control whose effects are interleaved.
  Then, scheduling is the process of interleaving those logical threads.

  M:N Threading

  A1 A2 A3 A4
                    -->     A1 B2 B3 C4 A3 A4
  B1 B2 B3 B4
                    -->     B1 C1 A2 C2 C3 B4
  C1 C2 C3 C4

  -->    we can have more levels before

  We interleave, interleave, interleave till we hit the bottom layer where then we have things in parallel.

  Threads are abstractions.

  {
    case study: node.js (1:N multi-threaded platform)
      So, our Node.js applications are single-threaded, right? Well, kind of.
      Actually, we can run things in parallel, but we don’t create threads, and we don’t sync them. The virtual machine
      and the operating system run the I/O in parallel for us, and when it’s time to send data back to our JavaScript code,
      the JavaScript part is the one that runs in a single thread.
      In other words, everything runs in parallel except for our JavaScript code.
  }
   */

  /*
  A concurrent program sometimes becomes a parallel program when it’s running in a multicore environment.
  A parallel process could be a distributed process when it’s running on multiple network nodes.
   */

  /*
  Problem with low level concurrency primitives, the threads

  The issue with using threads is it’s a low level of abstraction for concurrency. Threads are too close to hardware and
  represent the way work is scheduled or executed by the CPU.

  Threading encourages shared state concurrency, and it’s hard to make programs run in parallel because of locks,
  semaphores, and dependencies between threads.

  we need abstraction over threads, which then avoids giving more responsibility to the developer.
   */

  /*
  Asynchronous computations, "call me maybe"

  An Asynchronous computation/process is any task, thread, process, node somewhere on the network that:

  1. executes outside of your program’s main flow or from the point of view of the caller, it doesn’t execute on the current call-stack
  2. receives a callback that will get called once the result is finished processing
  3. it provides no guarantee about when the result is signaled, no guarantee that a result will be signaled at all

  A process that continues it's execution in a different place or time than the one it started in.

  Async Boundary- Resuming somewhere else

  Async processes resume somewhere else after an async boundary.

  Logical threads abstract async processes as sequences of discrete steps.

  Blocking then means suspending one layer down (e.g Future.get)
   */

  /*
  CPS Continuation Passing Style

  you have something that takes a callback and resume somewhere else calling the callback and so on
  */

  def sumRaw(a: Int, b: Int): Int = a + b

  def productRaw(a: Int, b: Int): Int = a * b

  def sum(a: Int, b: Int)(f: Int => Unit): Unit = {
    f(a + b)
  }

  def product(a: Int, b: Int)(f: Int => Unit): Unit = {
    f(a * b)
  }

  def printAOrBAndC(a: Int, b: Int, c: Int): Unit = {
    product(b, c) { bc =>
      sum(a, bc) { abc =>
        println(abc)
      }
    }
  }

  type Async[A] = (Try[A] => Unit) => Unit

  val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  def double(n: Int): Async[Int] =
    onComplete => {
      ec.execute(() => onComplete(Success(n * 2)))
    }

  /*
  problem: sequential
   */
  def quadruple(n: Int): Async[Int] =
    onComplete =>
      double(n).apply {
        case Success(addedOne) => double(addedOne).apply(onComplete)
        case f@Failure(_) => onComplete(f)
      }

  /*
  problem: nondeterminism, no ordering guarantee here
   */
  def quadrupleInParallel(n: Int): Async[Int] =
    onComplete => {
      val result = new AtomicInteger() // access to this state has to be synchronized

      double(n).apply { tryResult =>
        tryResult.foreach { doubled =>
          val afterDoubleSave = result.addAndGet(doubled)
          if (afterDoubleSave != doubled) onComplete(Success(afterDoubleSave))
        }
      }

      double(n).apply { tryResult =>
        tryResult.foreach { doubled =>
          val afterDoubleSave = result.addAndGet(doubled)
          if (afterDoubleSave != doubled) onComplete(Success(afterDoubleSave))
        }
      }
    }

  def map[A, B](fa: Async[A], f: A => B): Async[B] = {
    onComplete => fa.apply(tryA => onComplete(tryA.map(f)))
  }

  /*
  Applicative
   */
  def mapBoth[A, B, R](fa: Async[A], fb: Async[B])(f: (A, B) => R): Async[R] =
    onComplete => {
      val state = new AtomicReference[Option[Either[A, B]]](None)

      fa.apply { tryResult =>
        tryResult.foreach { a =>
          val didSet = state.compareAndSet(None, Some(Left(a)))
          if (!didSet) onComplete(Success(f(a, state.get.get.right.get)))
        }
      }

      fb.apply { tryResult =>
        tryResult.foreach { b =>
          val didSet = state.compareAndSet(None, Some(Right(b)))
          if (!didSet) onComplete(Success(f(state.get.get.left.get, b)))
        }
      }
    }

  def sequence[A](list: List[Async[A]]): Async[List[A]] = {
    val empty: Async[List[A]] = onComplete => onComplete(Success(List.empty[A]))

    list.foldLeft(empty) { (accA, asyncA) =>
      mapBoth(asyncA, accA)(_ :: _)
    }
  }

  def printResult[A](tryResult: Try[A]): Unit = println(s"Result: $tryResult")

  def main(args: Array[String]): Unit = {
    double(20).apply(printResult)

    quadruple(20).apply(printResult)

    quadrupleInParallel(20).apply(printResult)

    Thread.sleep(1000)
  }

  /*
  Software Transactional Memory- optimistic locking

  Optimistic locking is used when you don't expect many collisions. It costs less to do a normal operation but if the
  collision DOES occur you would pay a higher price to resolve it as the transaction is aborted.

  Pessimistic locking is used when a collision is anticipated. The transactions which would violate synchronization are simply blocked.
   */

  /*
  Coming back

  kernel threads vs user threads

  Kernel threads are independent of the ongoing processes and are executed by the operating system, expensive to create
  and context switch
  User thread (e.g. jvm threads) are implemented by users, not recognized by the OS, they map to kernel threads (M:N), blocking
  one doesn't block the processor (*kernel)

  Layers

  OS processes, M:N with processors, own execution state, own memory space
  User/JVM Threads, M: N with processes, own execution state, shared memory space
  Fibers/Green Threads: M:N with Threads, shared execution state, shared memory space

  Blocking Fiber doesn't block the underlying thread

  Preemptive vs Cooperative Scheduling

  In a preemptive model, the operating system's thread scheduler is allowed to step in and hand control from one thread
  to another at any time(tasks can be forcibly suspended).
  In cooperative model, once a thread is given control it continues to run until it explicitly yields control(handover
  control of CPU to the next task) or until it blocks.

  Fibers do M:N Cooperative Scheduling against a thread pool which is the scheduler.
  */

}
