package day14

import common.AdventProblemSolution

import scala.annotation.tailrec

object Solution extends AdventProblemSolution[Int, Int]{
  override def part1(args: List[String]): Int = {
    val reservoir = Reservoir.fromStrings(args)

    val reservoirFullOfSand = fillSand(reservoir)

    reservoirFullOfSand.mkString.foreach(println)
    reservoirFullOfSand.totalSandUnits
  }

  override def part2(args: List[String]): Int = {
    val reservoir = Reservoir.fromStrings(args).copy(withFixedBottom = true)

    val reservoirFullOfSand = fillSand(reservoir)

    reservoirFullOfSand.mkString.foreach(println)
    reservoirFullOfSand.totalSandUnits
  }

  @tailrec
  private def fillSand(r: Reservoir): Reservoir = r.newReservoirWithASandUnit match {
    case Some(nr) => {
      if (nr.totalSandUnits % 100 == 0)
      println(s"Completed Sand unit # ${nr.totalSandUnits}")
      fillSand(nr)
    }
    case None => r
  }
}
