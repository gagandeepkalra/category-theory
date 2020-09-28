package state

/**
 * A mutable state reference.
 *
 * Ops- Allocate, Write, Read
 *
 * Type S is only used for tagging, more like an authorization token to mutate or access the cell.
 *
 * The whole computation is referentially transparent because all mutable state is private and locally scoped.
 */
sealed trait STRef[S, A] {
  protected var cell: A

  def read: StateTag[S, A] = StateTag(cell)

  def write(a: A): StateTag[S, Unit] = {
    StateTag {
      cell = a
      ()
    }
  }
}

object STRef {
  def apply[S, A](a: A): StateTag[S, STRef[S, A]] = StateTag(new STRef[S, A] {
    var cell: A = a
  })
}
