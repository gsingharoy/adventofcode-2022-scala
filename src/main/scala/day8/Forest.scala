package day8

case class Forest(trees: List[Tree]) {

  /**
   *
   * @param tl
   * @return true if the tree is visible from any of the edges.
   */
  def isTreeVisibleToAnyOfTheEdges(tl: Tree): Boolean =
    isTreeVisibleFromLeft(tl) ||
      isTreeVisibleFromRight(tl) ||
      isTreeVisibleFromNorth(tl) ||
      isTreeVisibleFromSouth(tl)

  /**
   *
   * @param tl
   * @return true if the particular tree is visible from the left side
   */
  def isTreeVisibleFromLeft(tl: Tree): Boolean = tl.isVisibleToEdge(findTreesFromLeft(tl))

  /**
   *
   * @param tl
   * @return true if the particular tree is visible from the right side
   */
  def isTreeVisibleFromRight(tl: Tree): Boolean = tl.isVisibleToEdge(findTreesFromRight(tl))

  /**
   *
   * @param tl
   * @return true if the particular tree is visible from the north side
   */
  def isTreeVisibleFromNorth(tl: Tree): Boolean = tl.isVisibleToEdge(findTreesFromNorth(tl))

  /**
   *
   * @param tl
   * @return true if the particular tree is visible from the south side
   */
  def isTreeVisibleFromSouth(tl: Tree): Boolean = tl.isVisibleToEdge(findTreesFromSouth(tl))

  def findTreesFromLeft(tl: Tree): List[Tree] =
    trees.filter(t => t.col < tl.col && t.row == tl.row)

  private def findTreesFromRight(tl: Tree): List[Tree] =
    trees.filter(t => t.col > tl.col && t.row == tl.row)

  private def findTreesFromNorth(tl: Tree): List[Tree] =
    trees.filter(t => t.col == tl.col && t.row < tl.row)

  private def findTreesFromSouth(tl: Tree): List[Tree] =
    trees.filter(t => t.col == tl.col && t.row > tl.row)
}

object Forest {

  /**
   *
   * @param strings two dimensional string based on the question's input which helps in building the forest
   * @return
   */
  def constructFromStrings(strings: List[String]): Forest = {
    val trees: List[Tree] = strings.zipWithIndex.toList.flatMap(row => {
      row
        ._1
        .split("")
        .toList
        .flatMap(_.toIntOption)
        .zipWithIndex
        .map(col => Tree(col = col._2, row = row._2, height = col._1))
    })
    Forest(trees)
  }
}

