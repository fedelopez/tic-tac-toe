package cat.pseudocodi.ttt

/**
  * @author fede
  */
case class Point(row: Int, col: Int)

case class Grid(rows: List[Row]) {
  def cellAt(row: Int, column: Int): Option[Cell] = {
    if (row < 0 || column < 0 || rows.size <= row || rows(row).cells.size <= column) Option.empty
    else Option(rows(row).cells(column))
  }

  def setCellAt(cell: Cell, p: Point): Grid = {
    val newCols: List[Cell] = rows(p.row).cells.updated(p.col, cell)
    new Grid(rows.updated(p.row, Row(newCols)))
  }

  def gameStatus(): GameStatus = {
    if (rowsHaveAll(Cross, 0)) CrossWins
    else if (rowsHaveAll(Nought, 0)) NoughtWins
    else if (columnsHaveAll(Cross, 0)) CrossWins
    else if (columnsHaveAll(Nought, 0)) NoughtWins
    else if (rows.exists(row => row.cells.contains(Empty))) Playing
    else Draw
  }

  private def columnsHaveAll(cell: Cell, columnIndex: Int): Boolean = {
    if (columnIndex >= rows.length) false
    else {
      val count: Int = rows.foldLeft(0)((i: Int, row: Row) => if (row.cells(columnIndex) == cell) i + 1 else i)
      if (count == rows.length) true
      else columnsHaveAll(cell, columnIndex + 1)
    }
  }

  private def rowsHaveAll(cell: Cell, rowIndex: Int): Boolean = {
    if (rowIndex >= rows.length) false
    else {
      val count: Int = rows(rowIndex).cells.foldLeft(0)((i: Int, c: Cell) => if (c == cell) i + 1 else i)
      if (count == rows.length) true
      else rowsHaveAll(cell, rowIndex + 1)
    }
  }
}

object Grid {
  def apply() = {
    val empty: Row = new Row(List(Empty, Empty, Empty))
    new Grid(List(empty, empty, empty))
  }
}

case class Row(cells: List[Cell])

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