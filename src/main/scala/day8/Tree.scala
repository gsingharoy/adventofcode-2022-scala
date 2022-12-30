package day8

import scala.annotation.tailrec

case class Tree(col: Int, row: Int, height: Int) {

  /** function to figure out if this tree is visible to the edge
    * @param treesInFront
    *   trees in the order which are in front of this tree. Please note that they have to be called
    *   in the direction they are in front.
    * @return
    */
  def isVisibleToEdge(treesInFront: List[Tree] = List.empty): Boolean =
    treesInFront.isEmpty || !treesInFront.exists(_.height >= height)

  /** Find number of visible trees in the front. If it is in the edge it will return 0
    *
    * @param treesInFront
    *   tree List in the order they are in front. This the last element of the list is the most
    *   adjacent to the tree
    * @return
    */
  def visibleTreesInFront(treesInFront: List[Tree] = List.empty): Int = {
    if (treesInFront.isEmpty)
      return 0

    @tailrec
    def f(trees: List[Tree], result: Int = 0): Int = trees match {
      case Nil => result
      case head :: tail if head.height >= height =>
        result + 1 // hit a tree which is longer, time to stop!
      case head :: tail => f(tail, result + 1)
    }

    f(treesInFront.reverse)
  }

}
