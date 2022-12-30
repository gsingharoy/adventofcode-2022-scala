package day4

import scala.annotation.tailrec

case class Assignment(elf1Shelves: List[Int], elf2Shelves: List[Int]) {

  /** Checks if the assignment is overlapping with the other assignment in the list
    *
    * @return
    */
  lazy val hasCompleteOverlap: Boolean = {

    @tailrec
    def f(l: List[Int], matchingList: List[Int], result: Boolean = false): Boolean = l match {
      case Nil => result
      case head :: tail =>
        if (matchingList.contains(head)) f(tail, matchingList, true)
        else false
    }

    f(elf1Shelves, elf2Shelves) || f(elf2Shelves, elf1Shelves)
  }

  /** Returns true if there is some work where there are overlaps
    */
  lazy val hasPartialOverlap: Boolean = {

    @tailrec
    def f(l: List[Int], matchingList: List[Int], result: Boolean = false): Boolean = l match {
      case Nil => result
      case head :: tail =>
        if (matchingList.contains(head)) true
        else f(tail, matchingList, result)
    }

    f(elf1Shelves, elf2Shelves) || f(elf2Shelves, elf1Shelves)
  }
}

object Assignment {

  /** Tries to construct an assignment from a string
    * @param str
    *   has to be of the format "6-7,10-90"
    * @return
    */
  def constructFromString(str: String): Option[Assignment] = str.split(",").toList match {
    case l if l.length != 2 => None
    case ass1 :: ass2 :: _ =>
      (AssignmentUtils.constructShelves(ass1), AssignmentUtils.constructShelves(ass2)) match {
        case (Some(s1), Some(s2)) => Some(Assignment(s1, s2))
        case _                    => None
      }
  }
}
