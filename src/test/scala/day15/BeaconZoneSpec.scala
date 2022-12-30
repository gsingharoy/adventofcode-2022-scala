package day15

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BeaconZoneSpec extends AnyFlatSpec with Matchers {

  "SensorBeaconPair.fromString" should "be able to parse from string" in {
    val result =
      SensorBeaconPair.fromString("Sensor at x=3, y=18: closest beacon is at x=-2, y=15").get

    result.sensor.pos shouldEqual Coordinate(3, 18)
    result.closestBeacon.pos shouldEqual Coordinate(-2, 15)
  }

  "SensorBeaconPair#beaconExclusionRanges" should "return ranges" in {
    val sensor = Sensor(Coordinate(8, 7))
    val beacon = Beacon(Coordinate(2, 10))
    val sbr    = SensorBeaconPair(sensor, beacon)

    sbr.beaconExclusionRanges(-1) shouldEqual List(
      YCoordinateRange(-1, 7, 9)
    )
    sbr.beaconExclusionRanges(-2) shouldEqual List(
      YCoordinateRange(-2, 8, 8)
    )

    sbr.beaconExclusionRanges(7) shouldEqual List(
      YCoordinateRange(7, -1, 7),
      YCoordinateRange(7, 9, 17)
    )

    sbr.beaconExclusionRanges(10) shouldEqual List(
      YCoordinateRange(10, 3, 14)
    )

    sbr.beaconExclusionRanges(11) shouldEqual List(
      YCoordinateRange(11, 3, 13)
    )
  }

  "BeaconZone.#beaconExclusionRanges" should "return the expected exclusion zones" in {
    val args = FileUtils.readFile("day15/sample")

    val beaconZone: BeaconZone = BeaconZone(args.flatMap(SensorBeaconPair.fromString))

    beaconZone.beaconExclusionRanges(10).map(_.totalCoordinates).sum shouldEqual 26
  }
}
