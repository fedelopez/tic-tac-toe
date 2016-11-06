package cat.pseudocodi.xox

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