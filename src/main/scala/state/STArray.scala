package state

sealed abstract class STArray[S, A: Manifest] {
  protected def cells: Array[A]

  def read(i: Int): StateTag[S, A] = StateTag(cells(i))

  def write(i: Int, a: A): StateTag[S, Unit] = StateTag {
    cells(i) = a
    ()
  }

  def size: StateTag[S, Int] = StateTag(cells.length)

  def view: StateTag[S, List[A]] = StateTag(cells.toList)

  def fill(kv: Map[Int, A]): StateTag[S, Unit] = {
    kv.foldLeft(StateTag[S, Unit](())) { case (acc, (k, v)) => acc.flatMap(_ => write(k, v)) }
  }

  def swap(i: Int, j: Int): StateTag[S, Unit] = {
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()
  }
}

object STArray {
  def apply[S, A: Manifest](length: Int, v: A): StateTag[S, STArray[S, A]] = StateTag(new STArray[S, A] {
    override val cells: Array[A] = Array.fill(length)(v)
  })

  def fromList[S, A: Manifest](ls: List[A]): StateTag[S, STArray[S, A]] = StateTag(new STArray[S, A] {
    override val cells: Array[A] = ls.toArray
  })
}
