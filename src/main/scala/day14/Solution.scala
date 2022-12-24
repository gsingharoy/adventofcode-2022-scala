package day14

import common.AdventProblemSolution

import scala.annotation.tailrec

object Solution extends AdventProblemSolution[Int, Int]{
  override def part1(args: List[String]): Int = {
    val reservoir = Reservoir.fromStrings(args)

    @tailrec
    def f(r: Reservoir): Reservoir = r.newReservoirWithASandUnit match {
      case Some(nr) => {
        //nr.mkString.map(println)
        println(s"Completed Sand unit # ${nr.totalSandUnits}")
        f(nr)
      }
      case None => r
    }

    val reservoirFullOfSand = f(reservoir)

    reservoirFullOfSand.mkString.map(println)
    reservoirFullOfSand.totalSandUnits
  }

  override def part2(args: List[String]): Int = ???
}
