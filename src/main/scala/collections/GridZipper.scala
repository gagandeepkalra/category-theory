package collections

import instances.zipper._

case class GridZipper[A](value: Zipper[Zipper[A]]) {
  def north: GridZipper[A] = GridZipper(value.moveLeft)
  def south: GridZipper[A] = GridZipper(value.moveRight)
  def east: GridZipper[A] = GridZipper(value.setFocus(value.extract.moveRight))
  def west: GridZipper[A] = GridZipper(value.setFocus(value.extract.moveLeft))

  def getAllNeighbours: List[A] = List()
}
