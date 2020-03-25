package collections

import instances.zipper._

/**
  * CoMonadic data structure
  */
case class GridZipper[A](value: Zipper[Zipper[A]]) {
  def north: GridZipper[A] = GridZipper(value.moveLeft)
  def south: GridZipper[A] = GridZipper(value.moveRight)
  def east: GridZipper[A] = GridZipper(value.map(_.moveRight))
  def west: GridZipper[A] = GridZipper(value.map(_.moveLeft))

  def getAllNeighbours: Set[GridZipper[A]] =
    (Set(
      north,
      south,
      east,
      west,
      north.west,
      north.east,
      south.west,
      south.east
    ) - this)

  def toList: List[List[A]] = value.map(_.toList).toList
}

object GridZipper {
  def fromSeq[A](lss: Seq[Seq[A]]): Option[GridZipper[A]] =
    Zipper.fromSeq(lss.flatMap(Zipper.fromSeq(_))).map(GridZipper(_))

}
