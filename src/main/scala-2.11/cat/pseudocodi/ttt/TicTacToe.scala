package cat.pseudocodi.ttt

import java.util.Scanner

/**
  * @author fede
  */
object TicTacToe {

  case class Point(row: Int, col: Int)

  case class Grid(rows: List[Row]) {
    def cellAt(row: Int, column: Int): Option[Cell] = {
      if (row < 0 || column < 0 || rows.size <= row || rows(row).columns.size <= column) Option.empty
      else Option(rows(row).columns(column))
    }
    def setCellAt(cell: Cell, p: Point): Grid = {
      val newCols: List[Cell] = rows(p.row).columns.updated(p.col, cell)
      new Grid(rows.updated(p.row, Row(newCols)))
    }
  }

  object Grid {
    def apply() = {
      val empty: Row = new Row(List(Empty, Empty, Empty))
      new Grid(List(empty, empty, empty))
    }
  }

  case class Row(columns: List[Cell])

  abstract case class Cell(value: String) {
    override def toString: String = value
  }

  object Cross extends Cell("X")

  object Nought extends Cell("0")

  object Empty extends Cell(" ")

  def main(args: Array[String]) {
    println("Welcome to Tic-Tac-Toe!")
    println("Press 's' to start a new game...")
    val sc: Scanner = new Scanner(System.in)
    var next = ""
    while (!next.contains("s")) {
      next = sc.nextLine()
    }
    val grid: Grid = Grid()
    drawGrid(grid)
    var xy: Option[Point] = coordinates(sc.nextLine())
    while (xy.isEmpty || !grid.cellAt(xy.get.row, xy.get.col).contains(Empty)) {
      xy = coordinates(sc.nextLine())
    }
    drawGrid(grid.setCellAt(Cross, xy.get))
  }

  def coordinates(rawValue: String): Option[Point] = {
    if (rawValue != null && rawValue.matches("\\d\\d")) Option(Point(rawValue.head.asDigit, rawValue.tail.toInt))
    else Option.empty
  }

  def drawGrid(grid: Grid) {
    println("")
    println(" ----------- ")
    println(s"| ${grid.rows.head.columns.mkString(" | ")} |")
    println(" ----------- ")
    println(s"| ${grid.rows(1).columns.mkString(" | ")} |")
    println(" ----------- ")
    println(s"| ${grid.rows(2).columns.mkString(" | ")} |")
    println(" ----------- ")
    println("")
  }
}
