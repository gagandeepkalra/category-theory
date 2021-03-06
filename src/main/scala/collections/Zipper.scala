package collections

import instances.stream._

case class Zipper[A](left: Stream[A], focus: A, right: Stream[A]) {

  def setFocus(newFocus: A): Zipper[A] = Zipper(left, newFocus, right)

  def maybeLeft: Option[Zipper[A]] =
    if (left.isEmpty) None
    else Some(Zipper(left.tail, left.head, focus #:: right))

  def moveLeft: Zipper[A] =
    if (left.isEmpty) this else Zipper(left.tail, left.head, focus #:: right)

  def maybeRight: Option[Zipper[A]] =
    if (right.isEmpty) None
    else Some(Zipper(focus #:: left, right.head, right.tail))

  def moveRight: Zipper[A] =
    if (right.isEmpty) this else Zipper(focus #:: left, right.head, right.tail)

  // stream where each element is the focus once
  def duplicateLefts: Stream[Zipper[A]] =
    unfold(this)(zipper => zipper.maybeLeft.map(l => l -> l))

  // stream where each element is the focus once
  def duplicateRights: Stream[Zipper[A]] =
    unfold(this)(zipper => zipper.maybeRight.map(r => r -> r))

  def toList: List[A] = left.reverse.toList ++ (focus :: right.toList)
}

object Zipper {
  def fromSeq[A](ls: Seq[A]): Option[Zipper[A]] =
    ls.headOption.map(Zipper(Stream.empty, _, ls.tail.toStream))
}
