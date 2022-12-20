package day8

case class Tree(col: Int, row: Int, height: Int) {

  /**
   * function to figure out if this tree is visible to the edge
   * @param treesInFront trees in the order which are in front of this tree. Please note that they have to be called
   *                     in the direction they are in front.
   * @return
   */
  def isVisibleToEdge(treesInFront: List[Tree] = List.empty): Boolean =
    treesInFront.isEmpty || !treesInFront.exists(_.height >= height)
}

