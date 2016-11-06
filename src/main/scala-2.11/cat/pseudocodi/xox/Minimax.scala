package cat.pseudocodi.xox

import java.lang.Math._

/**
  * @author fede
  */
object MiniMax {

  abstract class Tree

  case class Branch(nodes: List[Tree]) extends Tree

  case class Leaf(value: Int) extends Tree

  def miniMax(tree: Tree): Int = {
    def doIt(current: Tree, isMax: Boolean): Int = current match {
      case Branch(l) =>
        l.foldLeft(if (isMax) Int.MinValue else Int.MaxValue)((value: Int, t: Tree) => if (isMax) max(value, doIt(t, !isMax)) else min(value, doIt(t, !isMax)))
      case Leaf(x) => x
    }
    doIt(tree, isMax = true)
  }
}
