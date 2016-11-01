package cat.pseudocodi.ttt

import org.scalatest._

/**
  * @author fede
  */
class TicTacToeSpec extends FunSpec with BeforeAndAfter {

  var grid: Grid = null

  before {
    grid = new Grid()
  }

  describe("Grid") {
    it("should return the cell at a given position") {
      grid.setCellAt(Nought, Point(0, 2))
      grid.setCellAt(Cross, Point(0, 1))
      assert(grid.cellAt(Point(0, 0)).contains(Empty))
      assert(grid.cellAt(Point(0, 1)).contains(Cross))
      assert(grid.cellAt(Point(0, 2)).contains(Nought))
    }

    it("should return undefined if out of bounds") {
      assert(grid.cellAt(Point(0, -1)).isEmpty)
      assert(grid.cellAt(Point(-1, 0)).isEmpty)
      assert(grid.cellAt(Point(0, 3)).isEmpty)
      assert(grid.cellAt(Point(3, 0)).isEmpty)
    }

    it("should set the cell contents on the grid") {
      val point: Point = Point(2, 2)
      grid.setCellAt(Cross, point)
      assert(grid.cellAt(point).contains(Cross))
    }

    it("should return coordinates from string") {
      assert(Grid.toPoint("00") == Option(Point(0, 0)))
      assert(Grid.toPoint("13") == Option(Point(1, 3)))
    }

    it("should return empty option if cannot parse coordinates from string") {
      assert(Grid.toPoint("1a") == Option.empty)
    }

    it("should return empty option if cannot parse empty coordinates") {
      assert(Grid.toPoint("") == Option.empty)
      assert(Grid.toPoint(null) == Option.empty)
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
      grid.setCellAt(Nought, Point(0, 1))
      grid.setCellAt(Cross, Point(0, 2))
      grid.setCellAt(Cross, Point(1, 0))
      grid.setCellAt(Nought, Point(1, 1))
      grid.setCellAt(Nought, Point(1, 2))
      grid.setCellAt(Nought, Point(2, 0))
      grid.setCellAt(Cross, Point(2, 1))
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

    it("should return winner player with 3 crosses diagonally") {
      grid.setCellAt(Cross, Point(0, 0))
      grid.setCellAt(Cross, Point(1, 1))
      grid.setCellAt(Cross, Point(2, 2))
      grid.setCellAt(Nought, Point(2, 0))
      grid.setCellAt(Nought, Point(2, 1))
      assert(grid.gameStatus() == CrossWins)
    }

    it("should return winner player with 3 crosses diagonally right to left") {
      grid.setCellAt(Cross, Point(0, 2))
      grid.setCellAt(Cross, Point(1, 1))
      grid.setCellAt(Cross, Point(2, 0))
      grid.setCellAt(Nought, Point(2, 2))
      grid.setCellAt(Nought, Point(2, 1))
      assert(grid.gameStatus() == CrossWins)
    }
  }
}
