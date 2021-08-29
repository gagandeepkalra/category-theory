package collections

import algebra.CoMonad
import algebra.CoMonad.CoMonadOps
import collections.Store.IntStore

case class GridStore[A](underlying: IntStore[IntStore[A]], length: Int) {

  def value: A = underlying.current.current

  def north: Option[A] =
    if (underlying.s == 0) None
    else Some(underlying.valueAt(underlying.s - 1).current)

  def south: Option[A] =
    if (underlying.s == length - 1) None
    else Some(underlying.valueAt(underlying.s + 1).current)

  def east: Option[A] =
    if (underlying.current.s == length - 1) None
    else Some(underlying.current.valueAt(underlying.current.s + 1))

  def west: Option[A] =
    if (underlying.current.s == 0) None
    else Some(underlying.current.valueAt(underlying.current.s - 1))

  def northWest: Option[A] =
    if (underlying.s == 0) None
    else {
      val north = underlying.valueAt(underlying.s - 1) // one row up

      val column = underlying.current.s // current column

      if (column == 0) None
      else Some(north.valueAt(column - 1))
    }

  def northEast: Option[A] =
    if (underlying.s == 0) None
    else {
      val north = underlying.valueAt(underlying.s - 1) // one row up

      val column = underlying.current.s // current column

      if (column == length - 1) None
      else Some(north.valueAt(column + 1))
    }

  def southWest: Option[A] =
    if (underlying.s == length - 1) None
    else {
      val south = underlying.valueAt(underlying.s + 1) // one row down

      val column = underlying.current.s // current column

      if (column == 0) None
      else Some(south.valueAt(column - 1))
    }

  def southEast: Option[A] =
    if (underlying.s == length - 1) None
    else {
      val south = underlying.valueAt(underlying.s + 1) // one row down

      val column = underlying.current.s // current column

      if (column == length - 1) None
      else Some(south.valueAt(column + 1))
    }

  def getAllNeighbourValues: List[A] =
    List(
      north,
      south,
      east,
      west,
      northWest,
      northEast,
      southWest,
      southEast
    ).flatten

  def toSeq(rng: Range): Seq[Seq[A]] = {
    rng.map(col => {
      val row = underlying.f(col)
      rng.map(row.valueAt)
    })
  }

  def atFocusRow(row: Int): GridStore[A] =
    GridStore(underlying.atFocus(row), length)
  def atFocusCol(col: Int): GridStore[A] =
    GridStore(Store(underlying.f.andThen(_.atFocus(col)), underlying.s), length)

}

object GridStore {

  def fromSeq[A](lss: Seq[Seq[A]]): GridStore[A] =
    GridStore(Store.fromSeq(lss.map(Store.fromSeq)), lss.length)

  implicit val GridStoreCoMonad: CoMonad[GridStore] = new CoMonad[GridStore] {
    override def extract[A](fa: GridStore[A]): A =
      fa.value

    override def coFlatten[A](fa: GridStore[A]): GridStore[GridStore[A]] = {
      val grid: IntStore[IntStore[A]] = fa.underlying

      val currentColumn = grid.current.s
      val gridByColumn: IntStore[GridStore[A]] = Store(
        f = (column: Int) => fa.atFocusCol(column),
        s = currentColumn
      ) // each represents grid focused on different column

      val currentRow = grid.s
      val gridByRowThenColumn = Store(
        f = (row: Int) =>
          Store(gridByColumn.f.andThen(_.atFocusRow(row)), gridByColumn.s),
        s = currentRow
      ) // each represents grid focused on different row, by column focused

      GridStore(gridByRowThenColumn, fa.length)
    }

    override def map[A, B](fa: GridStore[A])(f: A => B): GridStore[B] =
      GridStore(fa.underlying.map(_.map(f)), fa.length)
  }
}
