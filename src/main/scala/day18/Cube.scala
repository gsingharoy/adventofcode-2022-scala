package day18

import scala.annotation.tailrec

case class Cube(origin: (Int, Int, Int), edge: Int = 1) {

  private lazy val sides: List[List[(Int, Int, Int)]] = {
    val x = origin._1
    val y = origin._2
    val z = origin._3
    List(
      List((x, y, z), (x + edge, y, z), (x + edge, y, z + edge), (x, y, z + edge)).sorted,
      List(
        (x + edge, y, z),
        (x + edge, y + edge, z),
        (x + edge, y + edge, z + edge),
        (x + edge, y, z + edge)
      ).sorted,
      List(
        (x + edge, y + edge, z),
        (x, y + edge, z),
        (x, y + edge, z + edge),
        (x + edge, y + edge, z + edge)
      ).sorted,
      List((x, y + edge, z), (x, y, z), (x, y, z + edge), (x, y + edge, z + edge)).sorted,
      List(
        (x, y, z + edge),
        (x + edge, y, z + edge),
        (x + edge, y + edge, z + edge),
        (x, y + edge, z + edge)
      ).sorted,
      List((x, y, z), (x + edge, y, z), (x + edge, y + edge, z), (x, y + edge, z)).sorted
    )
  }

  def hasAdjacentSides(that: Cube): Boolean = {

    // condition if we know the origin is more than the unit of edge

    if (
      (this.origin._1 - that.origin._1).abs > edge + 2 ||
      (this.origin._2 - that.origin._2).abs > edge + 2 ||
      (this.origin._2 - that.origin._2).abs > edge + 2
    )
      return false // do not even attempt to match. The cube is too far away

    @tailrec
    def c(currSides: List[List[(Int, Int, Int)]]): Boolean = currSides match {
      case Nil => false // no more slides left to match
      case head :: tail => {
        if (that.sides.contains(head))
          true // stop and return as it is possible to only have one adjacent side
        else c(tail)
      }

    }

    c(this.sides)
  }
}

object Cube {

  def fromString(str: String): Option[Cube] = str match {
    case s"${sx},${sy},${sz}" =>
      (sx.toIntOption, sy.toIntOption, sz.toIntOption) match {
        case (Some(x), Some(y), Some(z)) => Some(Cube((x, y, z)))
        case _                           => None
      }
    case _ => None
  }
}
