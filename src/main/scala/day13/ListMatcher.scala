package day13

object ListMatcher {
  import day13.ListElem.{ isInSingleBracket, isSingleList, removeBrackets, breakStr, addBrackets }

  def compareStrLists(leftStr: String, rightStr: String): Boolean = {


    /**
     * This method analyzes the breadth of a list.
     * @param lList
     * @param rList
     * @return 1 where the items are in order. -1 when the items are not in order. 0 when the items are not enough to be found a result
     */
    def breadthF(lList: List[String], rList: List[String]): Int = (lList, rList) match {
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
            Some(depthF(l, r))
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
        result.getOrElse(breadthF(lTail, rTail))
      }
    }

    def depthF(lstr: String, rstr: String): Int = {
      println(s"compare ${lstr} vs ${rstr}")
      (lstr, rstr) match {
        case (l, r) if isInSingleBracket(l) && isInSingleBracket(r)  =>
          // both the elements are in single brackets, time to call it again
          depthF(removeBrackets(l), removeBrackets(r))
        case (l, r) if isInSingleBracket(l) && r.nonEmpty && isSingleList(r)  =>
          // add a correction
          depthF(l, addBrackets(r))
        case (l, r) if isInSingleBracket(r) && l.nonEmpty && isSingleList(l) =>
          // add a correction
          depthF(addBrackets(l), r)
        case (lList , rList) =>
          // this is where we are happy with the strings and they would be started to be compared
          breadthF(breakStr(lList), breakStr(rList))
      }
    }
    depthF(leftStr, rightStr) >= 0
  }





}


