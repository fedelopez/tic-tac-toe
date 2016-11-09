package cat.pseudocodi.xox

import cat.pseudocodi.xox.MiniMax.{Branch, Leaf, Tree}

import scala.collection.mutable.ListBuffer

/**
  * @author fede
  */
case class Point(x: Int, y: Int)

class Grid {

  val size = 3

  private val grid: Array[Array[Cell]] = Array.fill(size, size)(Empty)

  def cellAt(p: Point): Option[Cell] = {
    if (p.x < 0 || p.y < 0 || size <= p.x || size <= p.y) Option.empty
    else Option(grid(p.x)(p.y))
  }

  def setCellAt(cell: Cell, p: Point): Unit = grid(p.x).update(p.y, cell)

  def gameStatus(): GameStatus = {
    if (rowsHaveAll(Cross, 0)) CrossWins
    else if (rowsHaveAll(Nought, 0)) NoughtWins
    else if (columnsHaveAll(Cross, 0)) CrossWins
    else if (columnsHaveAll(Nought, 0)) NoughtWins
    else if (diagonalsHaveAll(Cross)) CrossWins
    else if (diagonalsHaveAll(Nought)) NoughtWins
    else if (grid.flatten.contains(Empty)) Playing
    else Draw
  }

  def emptyCells(): Seq[Point] = {
    for {
      i <- 0 until size
      j <- 0 until size
      if cellAt(Point(i, j)).contains(Empty)
    } yield Point(i, j)
  }

  def neighbors(cell: Cell): Seq[Grid] = {
    if (gameStatus() != Playing) {
      List()
    } else {
      emptyCells().map((point: Point) => {
        val newGrid = copy()
        newGrid.setCellAt(cell, point)
        newGrid
      })
    }
  }

  private def copy(): Grid = {
    val copy = new Grid()
    for (i <- 0 until size; j <- 0 until size) {
      val p: Point = Point(i, j)
      copy.setCellAt(cellAt(p).get, p)
    }
    copy
  }

  private def rowsHaveAll(cell: Cell, rowIndex: Int): Boolean = {
    if (rowIndex >= size) false
    else {
      if (grid(rowIndex).forall(c => c == cell)) true
      else rowsHaveAll(cell, rowIndex + 1)
    }
  }

  private def columnsHaveAll(cell: Cell, columnIndex: Int): Boolean = {
    if (columnIndex >= size) false
    else {
      if (grid.forall(cells => cells(columnIndex) == cell)) true
      else columnsHaveAll(cell, columnIndex + 1)
    }
  }

  private def diagonalsHaveAll(cell: Cell): Boolean = {
    val flatten: Array[Cell] = grid.flatten
    val leftToRight = Range(2, size * size, 2).slice(0, size).forall(p => flatten(p) == cell)
    val rightToLeft = Range(0, size * size, 4).slice(0, size).forall(p => flatten(p) == cell)
    leftToRight || rightToLeft
  }
}

object Grid {
  def draw(grid: Grid) {
    println("")
    for (i <- 0 until grid.size) {
      println(" ----------- ")
      for (j <- 0 until grid.size) print(s"| ${grid.cellAt(Point(i, j)).get} ")
      print("|")
      println("")
    }
    println(" ----------- ")
    println("")
  }

  def toPoint(rawValue: String): Option[Point] = {
    if (rawValue != null && rawValue.matches("\\d\\d")) Option(Point(rawValue.head.asDigit, rawValue.tail.toInt))
    else Option.empty
  }

  def toTree(grid: Grid, cell: Cell): Tree = {
    def doIt(g: Grid, parent: Branch, cell: Cell): Unit = {
      for (n <- g.neighbors(cell)) {
        val tree: Tree = n.gameStatus() match {
          case Playing => Branch(ListBuffer())
          case CrossWins => Leaf(1)
          case NoughtWins => Leaf(-1)
          case Draw => Leaf(0)
        }
        parent.children += tree
        tree match {
          case branch: Branch => doIt(n, branch, if (cell == Cross) Nought else Cross)
          case _ => ()
        }
      }
    }
    val root: Branch = Branch(ListBuffer())
    doIt(grid, root, cell)
    root
  }
}

abstract case class Cell(value: String) {
  override def toString: String = value
}

object Cross extends Cell("X")

object Nought extends Cell("0")

object Empty extends Cell(" ")

abstract class GameStatus

object CrossWins extends GameStatus

object NoughtWins extends GameStatus

object Draw extends GameStatus

object Playing extends GameStatus