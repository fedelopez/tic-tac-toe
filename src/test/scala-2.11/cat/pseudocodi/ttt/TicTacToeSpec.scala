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

    it("should return a new grid when setting a cell") {
      val res: Grid = grid.setCellAt(Cross, Point(0, 0))
      assert(res.cellAt(0, 0).contains(Cross))
      assert(res.cellAt(0, 1).contains(Cross))
      assert(res.cellAt(0, 2).contains(Nought))
      assert(grid.cellAt(0, 0).contains(Empty))
    }
  }

  describe("coordinates") {
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
