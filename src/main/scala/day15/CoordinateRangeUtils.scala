package day15

import scala.annotation.tailrec

object CoordinateRangeUtils {

  def mergeRanges(ranges: List[YCoordinateRange]): List[YCoordinateRange] = {

    @tailrec
    def m(
        currRanges: List[YCoordinateRange],
        result: List[YCoordinateRange]
    ): List[YCoordinateRange] = currRanges match {
      case r if r.length < 2 => r ++ result
      case head1 :: head2 :: tail =>
        head1.merge(head2) match {
          case Some(newR) =>
            m(
              ((result :+ newR) ++ tail).sorted,
              List.empty
            ) // start over again with the new range and excluding the head1 and head 2
          case None =>
            m(head2 :: tail, result :+ head1) // no merge possible, move to the next iteration
        }
    }

    m(ranges.sorted, List.empty)
  }

  def splitWithPoints(
      ranges: List[YCoordinateRange],
      points: List[Coordinate]
  ): List[YCoordinateRange] = {
    @tailrec
    def f(currPoints: List[Coordinate], result: List[YCoordinateRange]): List[YCoordinateRange] =
      currPoints match {
        case Nil          => result
        case head :: tail => f(tail, result.flatMap(_.split(head)))
      }

    f(points.sorted, ranges.sorted)
  }

  def modifyRangesWithLimits(
      ranges: List[YCoordinateRange],
      xMin: Int,
      xMax: Int
  ): List[YCoordinateRange] = {

    @tailrec
    def f(
        currRanges: List[YCoordinateRange],
        result: List[YCoordinateRange]
    ): List[YCoordinateRange] = currRanges match {
      case Nil => result.sorted
      case head :: tail =>
        head.applyLimit(xMin, xMax) match {
          case Some(nr) => f(tail, nr :: result)
          case None     => f(tail, result)
        }
    }

    f(ranges, List.empty)
  }

}
