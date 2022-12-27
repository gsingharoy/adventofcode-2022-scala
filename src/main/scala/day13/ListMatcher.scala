package day13

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object ListMatcher {

  def compareStrLists(leftStr: String, rightStr: String): Boolean = {

    def addBrackets(str: String): String = s"[${str}]"

    /**
     * This method analyzes the breadth of a list.
     * @param lList
     * @param rList
     * @return 1 where the items are in order. -1 when the items are not in order. 0 when the items are not enough to be found a result
     */
    def breadth(lList: List[String], rList: List[String]): Int = (lList, rList) match {
      case (Nil, Nil) => 0
      case (Nil, _) => {
        println(s"return true as ran out of elements on left to compare")
        1
      }
      case (_, Nil) => {
        println(s"return false as ran out of elements on right to compare")
        -1
      }
      case (lh :: lTail, rh :: rTail) => {
        println(s"Attempting to check elements ${lh} and ${rh}")
        // both the elements are in brackets. Time to call this function again
        val result: Option[Int] = (lh, rh) match {
          case (l, r) if (isInSingleBracket(l) || isInSingleBracket(r)) => {
            // this string is not unblocked. We have to call it again
            Some(depth(l, r))
          }
          case (lStr, rStr) => (lStr.toIntOption, rStr.toIntOption) match {
            case (Some(l), Some(r)) if (l < r) => {
              println("result is true because left element is smaller than right")
              Some(1)
            } // the program will stop now
            case (Some(l), Some(r)) if (l > r) => {
              println("result is false because left is larger than right")
              Some(-1)
            } // the program will stop now
            case _ => None // no result was formed. Need to try the next byte
          }

        }
        result.getOrElse(breadth(lTail, rTail))
      }
    }
    def depth(lstr: String, rstr: String): Int = {
      println(s"compare ${lstr} vs ${rstr}")
      (lstr, rstr) match {
        case (l, r) if isInSingleBracket(l) && isInSingleBracket(r)  =>
          // both the elements are in single brackets, time to call it again
          depth(removeBrackets(l), removeBrackets(r))
        case (l, r) if isInSingleBracket(l) && r.nonEmpty && isSingleList(r)  =>
          // add a correction
          depth(l, addBrackets(r))
        case (l, r) if isInSingleBracket(r) && l.nonEmpty && isSingleList(l) =>
          // add a correction
          depth(addBrackets(l), r)
        case (lList , rList) =>
          // this is where we are happy with the strings and they would be started to be compared
          breadth(breakStr(lList), breakStr(rList))
      }
    }
    depth(leftStr, rightStr) >= 0
  }


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
        case '[' => f(tail, cache:+ h, depth + 1, result)
        case ']' => f(tail, cache:+ h, depth - 1, result)
        case ',' if depth > 0 => f(tail, cache:+ h, depth, result)
        case ',' => f(tail, List.empty, 0, result :+ cache.mkString )
        case c => f(tail, cache:+ c, depth, result)
      }
    }

    f(str.toList, List.empty, 0, List.empty)
  }

  def isSingleList(str: String): Boolean = breakStr(str).length <= 1

  def isInSingleBracket(str: String): Boolean = {

    @tailrec
    def f(s: List[Char], depth: Int): Boolean = s match {
      case Nil => depth == 0
      case head :: tail if head == '[' => f(tail, depth+1)
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
      case strList => f(strList,0)
    }
  }
  def removeBrackets(str: String): String = str match {
    case s"[${s}]" if isInSingleBracket(str) => s
    case s => s
  }

}


