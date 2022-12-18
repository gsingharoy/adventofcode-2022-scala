package day3

import scala.annotation.tailrec

trait Group {

  def duplicatedItems: List[Char]

  protected def listMatcher(originalList: List[Char], matchingLists: List[List[Char]]): List[Char] = {

    @tailrec
    def f(l1: List[Char], result: Set[Char]): Set[Char] =
    l1 match {
      case Nil => result
      case head :: tail => {
        if (!matchingLists.forall(_.contains(head)))
          f(tail,result)
        else
          f(tail, result + head)
      }
    }

    f(originalList,Set.empty).toList

  }
}
