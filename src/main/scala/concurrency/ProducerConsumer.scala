package concurrency

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

// Abstract producer
abstract class Producer[T](queue: BlockingQueue[T]) extends Runnable {
  def run() {
    while (true) {
      queue.put(produce())
    }
  }

  def produce(): T
}

// Abstract consumer
trait Consumer[T] extends Runnable {
  val queue: BlockingQueue[T]

  def run() {
    while (true) {
      val item = queue.take()
      consume(item)
    }
  }

  def consume(x: T): Unit
}

object ProducerConsumer extends App {
  val blockingQueue = new LinkedBlockingQueue[String]()

  // One thread for the producer
  val producer: Producer[String] = new Producer[String](blockingQueue) {
    var i = 0

    override def produce(): String = {
      i += 1
      i.toString
    }
  }
  new Thread(producer).start()

  class PrintingConsumer(id: Int) extends Consumer[String] {
    def consume(t: String): Unit = println(s"$id: received=$t")

    override val queue: BlockingQueue[String] = blockingQueue
  }

  (1 to 8).map(i => new Thread(new PrintingConsumer(i)).start())

}
