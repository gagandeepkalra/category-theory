package concurrency

import scala.collection.mutable

class MyBlockingQueue[A](capacity: Int) {
  val queue: mutable.Queue[A] = collection.mutable.Queue.empty[A]
  val MAX_SIZE: Int = capacity

  def enqueue(value: A): Unit = {
    queue.synchronized {
      while (queue.size >= MAX_SIZE) {
        queue.wait()
      }

      if (queue.isEmpty) {
        queue.notifyAll()
      }
      queue.enqueue(value)
    }
  }

  def dequeue: A = {
    queue.synchronized {
      while (queue.isEmpty) {
        queue.wait()
      }

      if (queue.size == MAX_SIZE) {
        queue.notifyAll()
      }
      queue.dequeue()
    }
  }

}

object MyBlockingQueue {
  def main(args: Array[String]): Unit = {
    val queue = new MyBlockingQueue[Int](5)

    val producers = Array.fill(2) {
      new Thread(() =>
        (1 to 10).foreach { i =>
          queue.enqueue(i)
          println(s"Queued $i")
        }
      )
    }

    producers.foreach(_.start)

    val consumer = new Thread(() =>
      (1 to 20).foreach { _ =>
        Thread.sleep(1000)
        println(s"Dequeue ${queue.dequeue}")
      }
    )

    consumer.start()

  }
}
