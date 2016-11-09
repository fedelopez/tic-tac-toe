package cat.pseudocodi.xox

import cat.pseudocodi.xox.MiniMax._
import org.scalatest._

/**
  * @author fede
  */
class TicTacToeSpec extends FunSpec with BeforeAndAfter {

  var grid: Grid = _

  before {
    grid = new Grid()
  }

  describe("Grid get and set cell at position") {
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
  }

  describe("Grid game status") {
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

  describe("Grid companion object") {
    it("should convert a grid into a game tree") {
      grid.setCellAt(Cross, Point(0, 0))
      grid.setCellAt(Nought, Point(0, 1))
      grid.setCellAt(Cross, Point(0, 2))
      grid.setCellAt(Nought, Point(1, 0))
      grid.setCellAt(Cross, Point(1, 1))
      grid.setCellAt(Cross, Point(1, 2))
      grid.setCellAt(Nought, Point(2, 1))
      grid.setCellAt(Nought, Point(2, 2))
      val tree: Tree = Grid.toTree(grid, Cross)
      val branch: Branch = tree.asInstanceOf[Branch]
      assert(branch.children.size == 1)
      assert(branch.children.head.asInstanceOf[Leaf].value == 1)
    }

    it("should convert an empty grid into a game tree") {
      val tree: Tree = Grid.toTree(grid, Cross)
      val branch: Branch = tree.asInstanceOf[Branch]
      assert(branch.children.size == 9)
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

  describe("Grid neighbors") {
    it("should return a new grid with the next possible moves for a player") {
      val neighbors: Seq[Grid] = grid.neighbors(Cross)
      assert(neighbors.size == 9)
    }

    it("should return empty list when the game is finished") {
      grid.setCellAt(Nought, Point(0, 1))
      grid.setCellAt(Cross, Point(0, 2))
      grid.setCellAt(Nought, Point(1, 0))
      grid.setCellAt(Cross, Point(1, 1))
      grid.setCellAt(Cross, Point(1, 2))
      grid.setCellAt(Nought, Point(2, 0))
      grid.setCellAt(Nought, Point(2, 1))
      grid.setCellAt(Nought, Point(2, 2))
      val neighbors: Seq[Grid] = grid.neighbors(Cross)
      assert(neighbors.isEmpty)
    }
  }

  describe("Grid empty cells") {
    it("should return a list of all the empty cells") {
      val actual: Seq[Point] = grid.emptyCells()
      assert(actual.size == 9)
      for (i <- 0 until 3; j <- 0 until 3) {
        assert(actual.contains(Point(i, j)))
      }
    }
  }
}
