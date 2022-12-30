package day15

import common.AdventProblemSolution

import scala.annotation.tailrec

object Solution extends AdventProblemSolution[Int, Long] {

  override def part1(args: List[String]): Int = {
    val beaconZone: BeaconZone = BeaconZone(args.flatMap(SensorBeaconPair.fromString))

    beaconZone.beaconExclusionRanges(2000000).map(_.totalCoordinates).sum
  }

  override def part2(args: List[String]): Long = {
    val beaconZone: BeaconZone = BeaconZone(args.flatMap(SensorBeaconPair.fromString))

    @tailrec
    def calc(y: Int): Option[Coordinate] = {
      if (y >= 4000000) None // break as y limit has reached
      val currRanges: List[YCoordinateRange] = beaconZone.rangesWithoutDistressSignal(y)
      CoordinateRangeUtils.modifyRangesWithLimits(currRanges, 0, 4000000) match {
        case r if r.length < 2 => calc(y + 1) // no gaps found. Move to next y value
        case _ :: tail :: _ => {
          println(s"Distress signal identified to be coming from (${tail.xStart - 1}, ${y})")
          Some(Coordinate(tail.xStart - 1, y))
        } // this is not entirely fool proof, but should be good for the example
      }
    }

    calc(0).map(c => (4000000L * c.x) + c.y).getOrElse(-1L)
  }
}
