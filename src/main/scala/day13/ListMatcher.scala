package day13

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object ListMatcher {
  import day13.ListElem.{ isInSingleBracket, getListElements, addBrackets }

  def compareStrLists(leftStr: String, rightStr: String): Boolean = {

    /**
     * These are mutable variables. The idea is to keep a track of all visited elements and also mark the ones which are
     * evaluated
     */
    val visitedElements: ArrayBuffer[ListElemPosition] = ArrayBuffer()
    val evaluatedElements: ArrayBuffer[ListElemPosition] = ArrayBuffer()

    /**
     * Sub function to add the element as discovered.
     *
     * @param elem
     */
    def addDiscoveredElements(elem: ListElem): Unit =
      if (!visitedElements.contains(elem.pos)) {
        visitedElements += elem.pos
        visitedElements ++= elem.children.map(_.pos)
      }


    /**
     * Finds all the unevaluated element tries of two strings within one depth
     *
     * @param lStr
     * @param rStr
     * @param currPos
     * @return
     */
    def unEvaluatedItems(lStr: String,
                         rStr: String,
                         currPos: ListElemPosition): (List[String], List[String], List[ListElemPosition]) = {

      @tailrec
      def f(l: List[String],
            r: List[String],
            c: List[ListElemPosition]): (List[String], List[String], List[ListElemPosition]) = (l, r, c) match {
        case (l, r, c) if (l.isEmpty || r.isEmpty) => (l, r, c)
        case (_, _, Nil) => (Nil, Nil, Nil)
        case (lh :: lt, rh :: rt, ceh :: cet) => if (evaluatedElements.contains(ceh))
          f(lt, rt, cet)
        else (lh :: lt, rh :: rt, ceh :: cet)
      }

      f(getListElements(lStr), getListElements(rStr), ListElem(lStr, currPos).children.map(_.pos))
    }


    def addEvaluatedElements(currPos: ListElemPosition): Unit = {

      /**
       * This sub function attempts to mark the parent as evaluated if all of it's children are evaluated
       *
       * @param cp
       * @return
       */
      def markP(cp: ListElemPosition): Unit = {
        val visitedChildrenCount: Int = visitedElements.distinct
          .count(_.depth == currPos.childDepth)
        val evaluatedChildrenCount: Int = evaluatedElements
          .filter(_.depth == currPos.childDepth).distinct.length

        if (visitedChildrenCount == evaluatedChildrenCount) {
          // mark the current position first
          evaluatedElements += cp
          //println(s"Marked Parent element position ${cp} as evaluated")
        }
      }

      // mark all breadth to be visited
      evaluatedElements ++= visitedElements.filter(_ == currPos)

      //println(s"Marked element positions ${visitedElements.filter(_ == currPos)} as evaluated")

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
    def breadthF(lList: List[String],
                 rList: List[String],
                 elements: List[ListElemPosition]): Int = (lList, rList, elements) match {
      case (Nil, Nil, _) => {
        //println("Ran out of elements to compare. Try the next elements")
        0
      }
      case (Nil, _, _) => {
        //println(s"return true as ran out of elements on left to compare")
        1
      }
      case (_, Nil, _) => {
        //println(s"return false as ran out of elements on right to compare")
        -1
      }
      case (lh :: lTail, rh :: rTail, cPos :: tailElements) => {
        //println(s"Attempting to check elements ${lh} and ${rh}")
        // both the elements are in brackets. Time to call this function again
        val result: Option[Int] = (lh, rh) match {
          case (l, r) if isInSingleBracket(l) && isInSingleBracket(r) => {
            // this string is not unblocked. We have to call it again
            Some(depthF(l, r, cPos))
          }
          case (l, r) if isInSingleBracket(l) => {
            // this string is not unblocked. We have to call it again with a correction
            Some(depthF(l, addBrackets(r), cPos))
          }
          case (l, r) if isInSingleBracket(r) => {
            // this string is not unblocked. We have to call it again with a correction
            Some(depthF(addBrackets(l), r, cPos))
          }
          case (lStr, rStr) => (lStr.toIntOption, rStr.toIntOption) match {
            case (Some(l), Some(r)) if (l < r) => {
              //println("result is true because left element is smaller than right")
              Some(1)
            } // the program will stop now
            case (Some(l), Some(r)) if (l > r) => {
              //println("result is false because left is larger than right")
              Some(-1)
            } // the program will stop now
            case _ => {
              addEvaluatedElements(cPos) // mark till current element to be evaluated
              None // no result was formed. Need to try the next byte
            }
          }
        }
        result.getOrElse(breadthF(lTail, rTail, tailElements))
      }
    }


        def depthF(lstr: String, rstr: String, currPos: ListElemPosition): Int = {
          //println(s"compare ${lstr} vs ${rstr}")
          //println(s"currently in depth ${currPos.depth} and breadth ${currPos.breadth}")
          addDiscoveredElements(ListElem(lstr,currPos))
          unEvaluatedItems(lstr, rstr, currPos) match {
            case (Nil, Nil, _) => {
              addEvaluatedElements(currPos) // the parent is marked as done
              0 // would have to be tried again
            }
            case (l, r, e) => breadthF(l, r, e)
          }
        }

        def exec(): Boolean = {
          val result = depthF(leftStr, rightStr, ListElemPosition.Start )
          if (result == 0)
            exec() // try again
          else
            result > 0
        }

      exec()

    }



}


