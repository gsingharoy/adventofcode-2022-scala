package day3

import scala.annotation.tailrec

case class Rucksack(firstCompartment: List[Char], secondCompartment: List[Char]) {

  /**
   * Returns items which are present in both the compartments
   */
  lazy val duplicatedItems: List[Char] = {
    @tailrec
    def f(l1: List[Char], result: Set[Char] = Set.empty): Set[Char] = l1 match {
      case Nil => result
      case head :: tail => {
        if (secondCompartment.contains(head))
         f(tail, result + head)
        else
          f(tail, result)
      }
    }

    f(firstCompartment).toList
  }
}

object  Rucksack {

  /**
   * Tries to construct a rucksack from an input string. Returns a None if an invalid string is passed
   *
   * @param str if vJrwpWtwJgWrhcsFMMfFFhFp is passed then it will return
   * @return
   */
  def constructFromString(str: String): Option[Rucksack] = {

    /**
     * Tail recursive function which checks if the list of characters only contains lower case or
     * upper case characters
     * @param s
     * @return
     */
    @tailrec
    def isValidStr(s: List[Char]): Boolean = s match {
      case Nil => true
      case head :: tail => head.toInt match {
        case i if ((i>=65 && i<=90) || (i >= 97 && i <= 122)) => isValidStr(tail)
        case _ => false
      }
    }


    if (str.isEmpty || str.length % 2 == 1)
      None // Here it means the input is empty or there aren't event elements to equally distribute items
    else {
      (str.toList.take(str.length / 2), str.toList.drop(str.length / 2)) match {
        case (a, b) if (isValidStr(a) && isValidStr(b)) => Some(Rucksack(a, b))
        case  _ => None
      }
    }
  }
}
