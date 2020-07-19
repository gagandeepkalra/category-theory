package concurrency

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import scala.collection.immutable

class ThreadPool(size: Int, queue: BlockingQueue[Runnable]) {
  self =>

  private val threads: immutable.Seq[Thread] = (1 to size).map(i => new Thread(
    new Consumer[Runnable] {
      override val queue: BlockingQueue[Runnable] = self.queue

      override def consume(x: Runnable): Unit = x.run()
    }
  ))
  threads.foreach(_.start())

  def submit(runnable: Runnable): Unit =
    queue.add(runnable)

}

object ThreadPool {
  def apply(size: Int, queue: BlockingQueue[Runnable] = new LinkedBlockingQueue[Runnable]()): ThreadPool =
    new ThreadPool(size, queue)
}

object ThreadPoolTest extends App {
  val pool = ThreadPool(8)

  (1 to 10).foreach(i => pool.submit(() => println(i)))
}