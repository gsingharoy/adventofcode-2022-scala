package day15

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int]{

  override def part1(args: List[String]): Int = {
    val beaconZone: BeaconZone = BeaconZone(args.flatMap(SensorBeaconPair.fromString))

    beaconZone.beaconExclusionPoints.count(_.pos.y == 2000000)
  }

  override def part2(args: List[String]): Int = ???
}
