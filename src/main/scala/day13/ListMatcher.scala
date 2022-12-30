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
    val discoveredElements: ArrayBuffer[ListElemPosition] = ArrayBuffer()
    val evaluatedElements: ArrayBuffer[ListElemPosition] = ArrayBuffer()
    var addedEvaluation: Boolean = false
    /**
     * Sub function to add the element as discovered.
     *
     * @param elem
     */
    def initializeDiscoveredElements(elem: ListElem): Unit = {
      discoveredElements += elem.pos
      elem.children.foreach(initializeDiscoveredElements)
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
        case (l, r, c) if (l.isEmpty || r.isEmpty || c.isEmpty) => (l, r, c)
        case (lh :: lt, rh :: rt, ceh :: cet) => if (evaluatedElements.contains(ceh))
          f(lt, rt, cet)
        else (lh :: lt, rh :: rt, ceh :: cet)
      }

      f(getListElements(lStr), getListElements(rStr), ListElem(lStr, currPos).children.map(_.pos))
    }


    /**
     * Function to mark elements as evaluated. This helps in not going through the same elements again.
     *
     * @param currPos
     */
    def markElementsAsEvaluated(currPos: ListElemPosition): Unit = {
      evaluatedElements += currPos

      // check if all the elements in the breadth are evaluated. If the answer is yes, then mark the parent as also evaluated
      currPos.parentPos.foreach( parentPos => {
        val evaluatedChildrenCountBreaths: List[Int] = evaluatedElements
          .filter(_.depth == parentPos.childDepth).distinct.map(_.breadth).toList
        val unvisitedChildrenCount: Int = discoveredElements.distinct
          .count(e => e.depth == parentPos.childDepth && !evaluatedChildrenCountBreaths.contains(e.breadth))


        if (unvisitedChildrenCount == 0)  // there are no unvisited children left. Mark the parent now as visited
          evaluatedElements += parentPos
      }
      )
    }


    /**
     * This method analyzes the breadth of a list.
     *
     * @param lList
     * @param rList
     * @return 1 where the items are in order.
     *         -1 when the items are not in order.
     *         0 when the items are not enough to be found a result
     */
    def breadthF(lList: List[String],
                 rList: List[String],
                 elements: List[ListElemPosition]): Int = (lList, rList, elements) match {
      case (Nil, Nil, _) => 0 // Ran out of elements to compare. Try the next elements
      case (Nil, _, _) => 1 // return true as ran out of elements on left to compare
      case (_, Nil, _) => -1 // return false as ran out of elements on right to compare

      case (lh :: lTail, rh :: rTail, cPos :: tailElements) => {
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
            case (Some(l), Some(r)) if (l < r) => Some(1) // the program will stop now as the bytes are in order
            case (Some(l), Some(r)) if (l > r) => Some(-1) // the program will stop now as the bytes are not in order
            case _ => {
              evaluatedElements += cPos // mark till current element to be evaluated
              None // no result was formed. Need to try the next byte
            }
          }
        }
        result.getOrElse(breadthF(lTail, rTail, tailElements))
      }
    }


    /**
     * Analyzes the depth of the list. If it finds sub elements then it calls the breadthF function to analyze the elements
     *
     * @param lstr
     * @param rstr
     * @param currPos
     * @return -1 when the bytes are not in order
     *         0 when no result has been found.
     *         1 when the bytes are in order
     */
        def depthF(lstr: String,
                   rstr: String,
                   currPos: ListElemPosition): Int = unEvaluatedItems(lstr, rstr, currPos) match {
            case (l, r, e) => breadthF(l, r, e) match {
              case 0 => {
                if (!addedEvaluation) {
                  markElementsAsEvaluated(currPos)
                  addedEvaluation = true
                }
                0
              }
              case r => r
            }
          }



      @tailrec
        def exec(counter: Int): Boolean = {
          if (counter >= 10)
            throw new RuntimeException(s"Could not figure out a solution after ${counter} retries")
          addedEvaluation = false
          (depthF(leftStr, rightStr, ListElemPosition.Root), evaluatedElements.contains(ListElemPosition.Root)) match {
            case (0, false) => exec(counter + 1)
            case (0, true) => true // ran out of elements to compare on the left side
            case (result, _) => result > 0
          }
        }

    initializeDiscoveredElements(ListElem.root(leftStr))
    exec(1)

    }



}


