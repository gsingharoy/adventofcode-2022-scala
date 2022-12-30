package day6

import scala.annotation.tailrec

case class Device(bytes: String, sizeOfSubroutine: Int = 4) {

  /** Find the start of the marker where there are at least 4 characters where they are unique
    * before the start of marker
    *
    * eg, for bvwbjplbgvbhsrlpgdmjqwftvncz, Some(5) would be returned
    */
  lazy val startOfMarker: Option[Int] = {

    @tailrec
    def f(byteList: List[Char], cache: List[Char], currMarker: Int = 1): Option[Int] = {
      byteList match {
        case Nil =>
          None // means we have exhausted the bytes without finding a valid start of marker
        case head :: tail =>
          cache :+ head match {
            case newCache
                if (newCache.length < sizeOfSubroutine && newCache.distinct.length == newCache.length) =>
              f(
                tail,
                newCache,
                currMarker + 1
              ) // We are still happy with the block we have selected and will move to the next
            case newCache
                if (newCache.length <= sizeOfSubroutine && newCache.distinct.length != newCache.length) =>
              f(
                newCache.drop(1) ++ tail,
                List.empty,
                currMarker - (newCache.length - 2)
              ) // We are now moving back our window to continue the calculation
            case newCache =>
              Some(currMarker) // a result is hit!!!
          }
      }
    }
    f(bytes.toList, List.empty, 1)
  }
}
