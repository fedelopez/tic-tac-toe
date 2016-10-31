package cat.pseudocodi.ttt

import java.util.Scanner

import scala.util.Random

/**
  * @author fede
  */
object TicTacToe extends App {

  println("Welcome to Tic-Tac-Toe!")
  println("Press any key to start a new game...")
  val sc: Scanner = new Scanner(System.in)
  sc.next()
  println("Player moves")
  var grid: Grid = Grid(3)
  Grid.draw(grid)
  var xy: Option[Point] = coordinates(sc.nextLine())
  while (xy.isEmpty || !grid.cellAt(xy.get.x, xy.get.y).contains(Empty)) {
    xy = coordinates(sc.nextLine())
  }
  grid = grid.setCellAt(Cross, xy.get)
  Grid.draw(grid)
  println("Computer moves")
  Thread.sleep(1200)
  var p: Point = Point(Random.nextInt(2), Random.nextInt(2))
  while (!grid.cellAt(p.x, p.y).contains(Empty)) {
    p = Point(Random.nextInt(2), Random.nextInt(2))
  }
  grid = grid.setCellAt(Nought, p)
  Grid.draw(grid)

  def coordinates(rawValue: String): Option[Point] = {
    if (rawValue != null && rawValue.matches("\\d\\d")) Option(Point(rawValue.head.asDigit, rawValue.tail.toInt))
    else Option.empty
  }
}
