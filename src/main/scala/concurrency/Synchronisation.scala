package concurrency

case class Account(id: Int, var balance: Int)

/*
For deadlock free world, we define order
 */
object Synchronisation {

  @scala.annotation.tailrec
  def transfer(from: Account, to: Account, amount: Int): Boolean = {
    if (from.id < to.id) {
      if (from.balance - amount >= 0 && to.balance + amount >= 0) {
        from.synchronized {
          println(s"Inside first block $from")
          Thread.sleep(5000)

          to.synchronized {
            println(s"Inside second block: $to")
            if (from.balance - amount >= 0 && to.balance + amount >= 0) {
              from.balance -= amount
              to.balance += amount
              true
            } else false
          }
        }
      } else false
    } else
      transfer(to, from, -amount)
  }

  def main(args: Array[String]): Unit = {
    val A = Account(1, 100)
    val B = Account(2, 120)

    val th1 = new Thread(() => transfer(A, B, 50))
    val th2 = new Thread(() => transfer(B, A, 25))

    th1.start()
    th2.start()
  }
}
