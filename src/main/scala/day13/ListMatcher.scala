package day13

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object ListMatcher {
  import day13.ListElem.{ isInSingleBracket, isSingleList, removeBrackets, breakStr, addBrackets }

  def compareStrLists(leftStr: String, rightStr: String): Boolean = {

    val visitedElements: ArrayBuffer[ListElem] = ArrayBuffer()
    val evaluatedElements: ArrayBuffer[ListElemPosition] = ArrayBuffer()

    def addVisitedElements(elem: ListElem): Unit =
      if (!visitedElements.contains(elem)) {
        visitedElements += elem
        visitedElements ++= elem.children
      }


    def unEvaluatedItems(lStr: String, rStr: String, currElem: ListElem): (List[String], List[String], List[ListElem]) = {

      @tailrec
      def f(l: List[String],
            r: List[String],
            c: List[ListElem]): (List[String], List[String], List[ListElem]) = (l, r, c) match {
        case (l, r, c) if (l.isEmpty || r.isEmpty) => (l, r, c)
        case (_, _, Nil) => (Nil, Nil, Nil)
        case (lh :: lt, rh :: rt, ceh :: cet) => if (evaluatedElements.contains(ceh.pos))
          f(lt, rt, cet)
        else (lh :: lt, rh :: rt, ceh :: cet)
      }

      f(breakStr(lStr), breakStr(rStr), currElem.children)
    }


    def addEvaluatedElements(currPos: ListElemPosition): Unit = {

      @tailrec
      def markP(cp: ListElemPosition): ListElemPosition =
        if (visitedElements.count(e => e.pos.depth == currPos.depth) ==
          evaluatedElements.filter(_.depth == currPos.depth).distinct.length) {
          // mark the current position first
          evaluatedElements += cp
          cp.parentPos match {
            case Some(np) => markP(np) // mark now the parent position
            case None => cp
          }
        } else {
          cp
        }

      // mark all breadth to be visited
      evaluatedElements += currPos

      // check if all the elements in the breadth are evaluated. If the answer is yes, then mark the parent as also evaluated
      currPos.parentPos.foreach(markP)
    }


    /**
     * This method analyzes the breadth of a list.
     *
     * @param lList
     * @param rList
     * @return 1 where the items are in order. -1 when the items are not in order. 0 when the items are not enough to be found a result
     */
    def breadthF(lList: List[String], rList: List[String], elements: List[ListElem]): Int = (lList, rList, elements) match {
      case (Nil, Nil, _) => 0
      case (Nil, _, _) => {
        println(s"return true as ran out of elements on left to compare")
        1
      }
      case (_, Nil, _) => {
        println(s"return false as ran out of elements on right to compare")
        -1
      }
      case (lh :: lTail, rh :: rTail, currElem :: tailElements) => {
        println(s"Attempting to check elements ${lh} and ${rh}")
        // both the elements are in brackets. Time to call this function again
        val result: Option[Int] = (lh, rh) match {
          case (l, r) if (isInSingleBracket(l) || isInSingleBracket(r)) => {
            // this string is not unblocked. We have to call it again
            Some(depthF(l, r, currElem))
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
            case _ => {
              addEvaluatedElements(currElem.pos) // mark till current element to be evaluated
              None // no result was formed. Need to try the next byte
            }
          }
        }
        result.getOrElse(breadthF(lTail, rTail, tailElements))
      }
    }


        def depthF(lstr: String, rstr: String, currElem: ListElem): Int = {
          println(s"compare ${lstr} vs ${rstr}")
          evaluatedElements.foreach(println)
          addVisitedElements(currElem)
          (lstr, rstr) match {
            case (l, r) if isInSingleBracket(l) && isInSingleBracket(r) =>
              // both the elements are in single brackets, time to call it again
              depthF(removeBrackets(l), removeBrackets(r), currElem)
            case (l, r) if isInSingleBracket(l) && r.nonEmpty && isSingleList(r) =>
              // add a correction
              depthF(l, addBrackets(r), currElem)
            case (l, r) if isInSingleBracket(r) && l.nonEmpty && isSingleList(l) =>
              // add a correction
              depthF(addBrackets(l), r, currElem)
            case (lList, rList) =>
              // this is where we are happy with the strings and they would be started to be compared
              unEvaluatedItems(lList, rList, currElem) match {
                case (_, _, Nil) => 0 // all elements have been evaluated
                case (l, r, e) => breadthF(l, r, e)
              }
          }
        }

        def exec(): Boolean = {
          val result = depthF(leftStr, rightStr, ListElem.root(leftStr) )
          if (result == 0)
            exec() // try again
          else
            result > 0
        }

      exec()

    }



}


