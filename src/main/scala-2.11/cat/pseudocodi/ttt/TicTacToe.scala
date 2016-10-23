package cat.pseudocodi.ttt

import java.util.Scanner

/**
  * @author fede
  */
object TicTacToe {

  case class Grid(rows: List[Row]) {

    def cellAt(row: Int, column: Int): Option[Cell] = {
      if (row < 0 || column < 0 || rows.size <= row || rows(row).columns.size <= column) Option.empty
      else Option(rows(row).columns(column))
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
    var line: String = sc.nextLine()
    while (!validPosition(line, grid)) {
      line = sc.nextLine()
      println(line)
    }
  }

  def validPosition(position: String, grid: Grid): Boolean = {
    val validInput = position matches "\\d\\d"
    if (validInput) {
      val cell = grid.cellAt(position.head.asDigit, position.tail.toInt)
      cell.contains(Empty)
    } else false
  }

  def drawGrid(grid: Grid) {
    println("")
    println(" ----------- ")
    println(s"| ${grid.rows(0).columns.mkString(" | ")} |")
    println(" ----------- ")
    println(s"| ${grid.rows(1).columns.mkString(" | ")} |")
    println(" ----------- ")
    println(s"| ${grid.rows(2).columns.mkString(" | ")} |")
    println(" ----------- ")
    println("")
  }
}
