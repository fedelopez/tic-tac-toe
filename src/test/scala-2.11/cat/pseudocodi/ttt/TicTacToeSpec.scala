package cat.pseudocodi.ttt

import cat.pseudocodi.ttt.TicTacToe._
import org.scalatest._

/**
  * @author fede
  */
class TicTacToeSpec extends FunSpec {

  val row = Row(List(Empty, Cross, Nought))
  val grid = new Grid(List(row, row, row))

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
  }

  describe("valid position") {
    it("should be valid when empty space on cell") {
      assert(validPosition("00", grid))
      assert(validPosition("10", grid))
      assert(validPosition("20", grid))
    }

    it("should be invalid when cross space on cell") {
      assert(!validPosition("12", grid))
      assert(!validPosition("22", grid))
      assert(!validPosition("32", grid))
    }
  }
}
