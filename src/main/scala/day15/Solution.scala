package day15

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int]{

  override def part1(args: List[String]): Int = {
    val beaconZone: BeaconZone = BeaconZone(args.flatMap(SensorBeaconPair.fromString))

    beaconZone.beaconExclusionRanges(2000000).map(_.totalCoordinates).sum
  }

  override def part2(args: List[String]): Int = ???
}
