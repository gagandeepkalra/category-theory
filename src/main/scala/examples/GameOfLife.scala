package examples

import collections.GridZipper
import instances.gridZipper._

/**
  * CoMonad demonstration
  */
object GameOfLife {

  /**
    * These rules, which compare the behavior of the automaton to real life, can be condensed into the following:
    *
    * Any live cell with two or three neighbors survives.
    * Any dead cell with three live neighbors becomes a live cell.
    * All other live cells die in the next generation. Similarly, all other dead cells stay dead.
    */
  def computeNextStateValue(g: GridZipper[Int]): Int = {
    val (neighbours, value) = (g.getAllNeighbours, g.extract)
    val sum = neighbours.foldLeft(0)(_ + _.extract)
    val result = (sum, value) match {
      case (s, 1) if s == 2 || s == 3 => 1
      case (3, 0)                     => 1
      case _                          => 0
    }
    result
  }

  def nextGrid(grid: GridZipper[Int]): GridZipper[Int] = {
    grid.coFlatMap(computeNextStateValue)
  }

  def main(args: Array[String]): Unit = {
    val board = List(List(0, 1, 0), List(1, 0, 0), List(0, 0, 1))

    val maybeInitial: Option[GridZipper[Int]] = GridZipper.fromSeq(board)

    maybeInitial.foreach(
      initial =>
        (1 to 10).foldLeft(initial)((acc, i) => {
          println(s"Generation $i\n${pretty(acc.toList)}")
          nextGrid(acc)
        })
    )
  }

  private def pretty(lss: List[List[Int]]): String = {
    lss.map(_.mkString(" ")).mkString("\n")
  }

}
