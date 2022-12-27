package day13

import scala.annotation.tailrec

/**
 * Represents a List element with it's current depth and breadth.
 * This case class is useful to count the elements which have been visited.
 *
 * @param elem
 * @param pos
 * @param parentPos
 */
case class ListElem(elem: String,
                    pos: ListElemPosition,
                    parentPos: Option[ListElemPosition]) {

  def children: List[ListElem] = {
    if(ListElem.isInSingleBracket(elem)) ListElem.breakStr(ListElem.removeBrackets(elem))
          .zipWithIndex.map( {
          case (s, i) =>
            ListElem(elem = s,
            pos = ListElemPosition(depth = this.pos.depth +1, breadth = i),
            parentPos = Some(this.pos))
        })
    else
      List.empty // there are no children as the current element is not a list

  }
}

object ListElem {
  /**
   * Breaks the string in such parts that each element is either in a bracket or is a full set in itself
   *
   * @param str
   * @return
   */
  def breakStr(str: String): List[String] = {

    if (str == "") return List.empty

    @tailrec
    def f(s: List[Char], cache: List[Char], depth: Int, result: List[String]): List[String] = s match {
      case Nil => result :+ cache.mkString
      case h :: tail => h match {
        case '[' => f(tail, cache :+ h, depth + 1, result)
        case ']' => f(tail, cache :+ h, depth - 1, result)
        case ',' if depth > 0 => f(tail, cache :+ h, depth, result)
        case ',' => f(tail, List.empty, 0, result :+ cache.mkString)
        case c => f(tail, cache :+ c, depth, result)
      }
    }

    f(str.toList, List.empty, 0, List.empty)
  }
  def isSingleList(str: String): Boolean = breakStr(str).length <= 1

  def removeBrackets(str: String): String = str match {
    case s"[${s}]" if isInSingleBracket(str) => s
    case s => s
  }


  def isInSingleBracket(str: String): Boolean = {

    @tailrec
    def f(s: List[Char], depth: Int): Boolean = s match {
      case Nil => depth == 0
      case head :: tail if head == '[' => f(tail, depth + 1)
      case head :: tail if head == ']' => {
        val newDepth = depth - 1
        if (newDepth == 0 && tail.nonEmpty)
          false
        else
          f(tail, newDepth)
      }
      case _ :: tail => f(tail, depth)
    }


    str.toList match {
      case Nil => false
      case head :: _ if head != '[' => false
      case strList => f(strList, 0)
    }
  }

  def addBrackets(str: String): String = s"[${str}]"

}

case class ListElemPosition(depth: Int, breadth: Int)

object  ListElemPosition {

  val Start: ListElemPosition = ListElemPosition(0, 0)
}