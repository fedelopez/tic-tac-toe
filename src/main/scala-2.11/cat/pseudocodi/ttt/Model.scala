package cat.pseudocodi.ttt

/**
  * @author fede
  */
case class Point(x: Int, y: Int)

case class Grid(size: Int) {

  private val grid: Array[Array[Cell]] = Array.fill(size, size)(Empty)

  def cellAt(row: Int, column: Int): Option[Cell] = {
    if (row < 0 || column < 0 || size <= row || size <= column) Option.empty
    else Option(grid(row)(column))
  }

  def setCellAt(cell: Cell, p: Point): Grid = {
    grid(p.x).update(p.y, cell)
    this
  }

  def gameStatus(): GameStatus = {
    if (rowsHaveAll(Cross, 0)) CrossWins
    else if (rowsHaveAll(Nought, 0)) NoughtWins
    else if (columnsHaveAll(Cross, 0)) CrossWins
    else if (columnsHaveAll(Nought, 0)) NoughtWins
    else if (grid.flatten.contains(Empty)) Playing
    else Draw
  }

  private def columnsHaveAll(cell: Cell, columnIndex: Int): Boolean = {
    if (columnIndex >= size) false
    else {
      if (grid.forall(cells => cells(columnIndex) == cell)) true
      else columnsHaveAll(cell, columnIndex + 1)
    }
  }

  private def rowsHaveAll(cell: Cell, rowIndex: Int): Boolean = {
    if (rowIndex >= size) false
    else {
      if (grid(rowIndex).forall(c => c == cell)) true
      else rowsHaveAll(cell, rowIndex + 1)
    }
  }
}

object Grid {
  def draw(grid: Grid) {
    println("")
    for (i <- 0 until grid.size) {
      println(" ----------- ")
      for (j <- 0 until grid.size) print(s"| ${grid.cellAt(i, j).get} ")
      print("|")
      println("")
    }
    println(" ----------- ")
    println("")
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