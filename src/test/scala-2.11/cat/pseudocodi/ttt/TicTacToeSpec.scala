package cat.pseudocodi.ttt

import cat.pseudocodi.ttt.TicTacToe._
import org.scalatest._

/**
  * @author fede
  */
class TicTacToeSpec extends FunSpec with BeforeAndAfter {

  var grid: Grid = null

  before {
    grid = Grid(3)
    grid.setCellAt(Nought, Point(0, 2))
    grid.setCellAt(Cross, Point(0, 1))
  }

  describe("Grid") {
    it("should return the cell at a given position") {
      assert(grid.cellAt(0, 0).contains(Empty))
      assert(grid.cellAt(0, 1).contains(Cross))
      assert(grid.cellAt(0, 2).contains(Nought))
    }

    it("should return undefined if out of bounds") {
      assert(grid.cellAt(0, -1).isEmpty)
      assert(grid.cellAt(-1, 0).isEmpty)
      assert(grid.cellAt(0, 3).isEmpty)
      assert(grid.cellAt(3, 0).isEmpty)
    }

    it("should return a new grid when setting a cell") {
      val res: Grid = grid.setCellAt(Cross, Point(0, 0))
      assert(res.cellAt(0, 0).contains(Cross))
      assert(res.cellAt(0, 1).contains(Cross))
      assert(res.cellAt(0, 2).contains(Nought))
    }
  }

  describe("Game Status") {
    it("should return playing when no draw or winner") {
      assert(grid.gameStatus() == Playing)
    }

    it("should return player 1 wins when 3 crosses horizontally") {
      Range(0, grid.size).indices.foreach(n => grid.setCellAt(Cross, Point(1, n)))
      grid.setCellAt(Nought, Point(2, 1))
      grid.setCellAt(Nought, Point(2, 2))
      assert(grid.gameStatus() == CrossWins)
    }

    it("should return player 2 wins when 3 noughts horizontally") {
      Range(0, grid.size).indices.foreach(n => grid.setCellAt(Nought, Point(1, n)))
      grid.setCellAt(Cross, Point(2, 1))
      grid.setCellAt(Cross, Point(2, 2))
      assert(grid.gameStatus() == NoughtWins)
    }

    it("should return draw when no player wins") {
      grid.setCellAt(Cross, Point(0, 0))
      grid.setCellAt(Cross, Point(0, 1))
      grid.setCellAt(Nought, Point(0, 2))
      grid.setCellAt(Nought, Point(1, 0))
      grid.setCellAt(Cross, Point(1, 1))
      grid.setCellAt(Cross, Point(1, 2))
      grid.setCellAt(Nought, Point(2, 0))
      grid.setCellAt(Nought, Point(2, 1))
      grid.setCellAt(Cross, Point(2, 2))
      assert(grid.gameStatus() == Draw)
    }

    it("should return winner player with 3 crosses vertically") {
      grid.setCellAt(Cross, Point(0, 0))
      grid.setCellAt(Cross, Point(1, 0))
      grid.setCellAt(Cross, Point(2, 0))
      grid.setCellAt(Nought, Point(2, 1))
      grid.setCellAt(Nought, Point(2, 2))
      assert(grid.gameStatus() == CrossWins)
    }

    //todo
    it("should return winner player with 3 crosses diagonally") {
      //      val row1 = Row(List(Cross, Nought, Nought))
      //      val row2 = Row(List(Empty, Cross, Empty))
      //      val row3 = Row(List(Empty, Empty, Cross))
      //      val grid = new Grid(List(row1, row2, row3))
      //      assert(grid.gameStatus() == CrossWins)
    }
  }

  describe("Coordinates") {
    it("should return coordinates from string") {
      assert(coordinates("00") == Option(Point(0, 0)))
      assert(coordinates("13") == Option(Point(1, 3)))
    }

    it("should return empty option if cannot parse coordinates from string") {
      assert(coordinates("1a") == Option.empty)
    }

    it("should return empty option if cannot parse empty coordinates") {
      assert(coordinates("") == Option.empty)
      assert(coordinates(null) == Option.empty)
    }
  }
}
