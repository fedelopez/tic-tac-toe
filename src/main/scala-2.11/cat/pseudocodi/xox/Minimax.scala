package cat.pseudocodi.xox

import java.lang.Math._

import scala.collection.mutable.ListBuffer

/**
  * @author fede
  */
object MiniMax {

  sealed abstract class Tree

  case class Branch(children: ListBuffer[Tree]) extends Tree

  case class Leaf(value: Int) extends Tree

  def miniMax(tree: Tree): Int = {
    def doIt(current: Tree, isMax: Boolean): Int = current match {
      case Branch(l) =>
        val seedValue: Int = if (isMax) Int.MinValue else Int.MaxValue
        l.foldLeft(seedValue)((v: Int, t: Tree) => if (isMax) max(v, doIt(t, !isMax)) else min(v, doIt(t, !isMax)))
      case Leaf(x) => x
    }
    doIt(tree, isMax = true)
  }
}
