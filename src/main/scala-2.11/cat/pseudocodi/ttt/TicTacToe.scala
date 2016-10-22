package cat.pseudocodi.ttt

import java.util.Scanner

/**
  * @author fede
  */
object TicTacToe {

  type Row = (String, String, String)

  abstract class Player(x: String)

  object Cross extends Player("X")

  object Nought extends Player("0")

  def main(args: Array[String]) {
    println("Welcome to Tic-Tac-Toe!")
    drawGrid(("X", "0", " "), (" ", "X", " "), ("0", " ", " "))
    println("Press 's' to start a new game...")
    val sc:Scanner = new Scanner(System.in)
    var next = ""
    while (!next.contains("s")) {
      next = sc.nextLine()
    }
    println(s"Press 'x' to be a cross, or any other key to be a nought...")
    val (player, computer) = players(sc.nextLine())
    println(s"Your selection is '${player}'.")
    println("To place your , type '11' ")
    drawGrid(("11", "12", "13"), ("21", "22", "23"), ("31", "32", "33"))

    player match {
      case Cross =>
      case Nought =>
    }

  }

  def players(selection: String): (Player, Player) = {
    if (selection.toLowerCase.contains("x")) (Cross, Nought)
    else (Nought, Cross)
  }

  def drawGrid(row1: Row, row2: Row, row3: Row) {
    println("")
    println("     --- --- --- ")
    println(s"    | ${row1._1} | ${row1._2} | ${row1._3} |")
    println("     --- --- --- ")
    println(s"    | ${row2._1} | ${row2._2} | ${row2._3} |")
    println("     --- --- --- ")
    println(s"    | ${row3._1} | ${row3._2} | ${row3._3} |")
    println("     --- --- --- ")
    println("")
  }
}
