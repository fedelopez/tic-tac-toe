package cat.pseudocodi.xox

import java.util.Scanner

import scala.util.Random._

/**
  * @author fede
  */
object TicTacToe extends App {

  println("Welcome to Tic-Tac-Toe!")
  println("Press any key to start a new game...")
  val sc: Scanner = new Scanner(System.in)
  sc.next()

  val grid: Grid = new Grid()
  Grid.draw(grid)

  while (grid.gameStatus() == Playing) {
    println("Player moves")
    val playerMove: Point = tryMove(() => Grid.toPoint(sc.nextLine()))
    grid.setCellAt(Cross, playerMove)
    Grid.draw(grid)
    if (grid.gameStatus() == Playing) {
      println("Computer moves")
      Thread.sleep(1000)
      val computerMove: Point = tryMove(() => Option(Point(nextInt(3), nextInt(3))))
      grid.setCellAt(Nought, computerMove)
      Grid.draw(grid)
    }
  }

  println(s"Game Finished, ${grid.gameStatus()}")

  def tryMove(f: () => Option[Point]): Point = {
    var nextMove: Option[Point] = f()
    while (nextMove.isEmpty || !grid.cellAt(nextMove.get).contains(Empty)) {
      nextMove = f()
    }
    nextMove.get
  }
}
