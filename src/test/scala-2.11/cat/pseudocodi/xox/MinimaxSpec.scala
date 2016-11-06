package cat.pseudocodi.xox

import cat.pseudocodi.xox.MiniMax._
import org.scalatest._

/**
  * @author fede
  */
class MiniMaxSpec extends FunSpec {

  describe("Minimax") {
    it("should return the leaf value on a single node tree") {
      assert(miniMax(Leaf(0)) == 0)
      assert(miniMax(Leaf(1)) == 1)
      assert(miniMax(Leaf(-1)) == -1)
    }

    it("should return the max leaf value on two level tree") {
      assert(miniMax(Branch(List(Leaf(-10), Leaf(-8)))) == -8)
    }

    it("should return the max leaf value on three level tree") {
      val left = Branch(List(Leaf(-8), Leaf(-5)))
      val right = Branch(List(Leaf(-10), Leaf(8)))
      assert(miniMax(Branch(List(left, right))) == -8)
    }
  }

}
