package common

import scala.annotation.tailrec

object ListUtils {

  /** Utility function which helps split a list at pivot values.
    *
    * @param args
    * @param pivotValue
    * @tparam T
    * @return
    *   eg, if List("aaa", "1", "bb", "cc", "89", "1", "90") is sent as an argument and the
    *   pivotValue is "1" List(List("aaa"), List("bb", "cc", "89"), List("90")) would be returned
    */
  def zipListByPivotValue[T](args: List[T], pivotValue: T): List[List[T]] = {

    @tailrec
    def f(iArgs: List[T], cache: List[T], result: List[List[T]]): List[List[T]] = iArgs match {
      case Nil => result :+ cache
      case head :: tail =>
        if (head == pivotValue)
          f(tail, List.empty, result :+ cache) // hit a pivot value, time to clear cache
        else
          f(tail, cache :+ head, result) // did not find zip value yet

    }

    f(args, List.empty, List.empty)
  }

  def transformToTuplePair[T](l: List[T]): Option[(T, T)] = l match {
    case l if l.length != 2 => None
    case h :: t :: _        => Some((h, t))
  }
}
