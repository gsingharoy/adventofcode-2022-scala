package day18

import scala.annotation.tailrec
import scala.collection.mutable

case class CubeStructure(cubes: List[Cube]) {

  /**
   * Returns the total number of exposed sides in the connected cube structure
   */
  lazy val exposedSides: Int = {
    val cubesExposedSidesMap: mutable.Map[(Int, Int, Int), Int] = mutable.Map.empty
    cubes.foreach({c =>
      cubesExposedSidesMap += (c.origin -> 6) // initialize all cubes with the 6 as exposed sides. This will be updated later
    })

    def reduceMapValue(c: Cube): Unit = cubesExposedSidesMap.get(c.origin) match {
        case Some(i)  => if (i > 0) {
            cubesExposedSidesMap -= c.origin
            cubesExposedSidesMap += (c.origin -> (i - 1))
          }
      }

    def checkAdjacentSides(cube1: Cube, cube2: Cube): Unit =
      if (cube1.hasAdjacentSides(cube2)){
        reduceMapValue(cube1)
        reduceMapValue(cube2)
      }


    @tailrec
    def check(curr: Cube, cache: List[Cube], total: List[Cube]): Int = cache match {
        case Nil => total match {
          case Nil => cubesExposedSidesMap.toList.map(_._2).sum
          case h :: t => check(curr = h, cache = t, total = t)
        }
        case head :: tail => {
          checkAdjacentSides(curr, head)
          check(curr = curr, cache = tail, total = total)
        }
      }


    this.cubes match {
      case  Nil => 0
      case head :: tail => check(head, tail, tail)
    }
  }
}


object CubeStructure{

  def fromStrings(strings: List[String]): CubeStructure = CubeStructure(strings.flatMap(Cube.fromString))
}