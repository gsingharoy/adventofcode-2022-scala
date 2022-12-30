package day18

import scala.annotation.tailrec

case class Cube(origin: (Int, Int, Int)) {

  private lazy val sides: List[List[(Int, Int, Int)]] = {
    val x = origin._1
    val y = origin._2
    val z = origin._3
    List(
      List((x, y, z), (x+1,y, z), (x+1, y, z + 1), (x, y, z+1)).sorted,
      List((x + 1, y, z), (x+1,y + 1, z), (x+1, y+1, z + 1), (x+ 1, y, z+1)).sorted,
      List((x + 1, y + 1, z), (x,y + 1, z), (x, y+1, z + 1), (x+ 1, y+1, z+1)).sorted,
      List((x , y + 1, z), (x,y,z), (x, y, z + 1), (x, y+1, z+1)).sorted,
      List((x , y , z + 1), (x + 1,y , z + 1), (x+1, y+1, z + 1), (x, y+1, z+1)).sorted,
      List((x , y , z), (x + 1,y , z ), (x+1, y+1, z ), (x, y+1, z)).sorted
    )
  }

  def hasAdjacentSides(that: Cube): Boolean = {

    // condition if we know the origin is more than the unit of edge

    if ((this.origin._1 - that.origin._1).abs > 2 ||
      (this.origin._2 - that.origin._2).abs > 2 ||
      (this.origin._2 - that.origin._2).abs > 2  )
      return false // do not even attempt to match. The cube is too far away

    @tailrec
    def c(currSides: List[List[(Int, Int, Int)]]): Boolean = currSides match {
      case Nil => false // no more slides left to match
      case head :: tail => {
        if (that.sides.contains(head)) true // stop and return as it is possible to only have one adjacent side
        else c(tail)
      }

    }

    c(this.sides)
  }
}

object Cube {

  def fromString(str: String): Option[Cube] = str match {
    case s"${sx},${sy},${sz}" => (sx.toIntOption, sy.toIntOption, sz.toIntOption) match {
      case (Some(x), Some(y), Some(z)) => Some(Cube((x,y,z)))
      case _ => None
    }
    case _ => None
  }
}



