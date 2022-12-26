package day15

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BeaconZoneSpec extends AnyFlatSpec with Matchers {
 "SensorBeaconPair#beaconExclusionPoints" should "return all the exlusion points lower or equal to the shortest distance between the beacon" in {
   val sensor = Sensor(Coordinate(8, 7))
   val beacon = Beacon(Coordinate(2, 10))
   val result = SensorBeaconPair(sensor, beacon).beaconExclusionPoints

   result.exists(r => r.pos.x == 8 && r.pos.y == 7) shouldEqual false
   result.exists(r => r.pos.x == 2 && r.pos.y == 10) shouldEqual false

   result.exists(r => r.pos.x == 8 && r.pos.y == 16) shouldEqual true
   result.exists(r => r.pos.x == 8 && r.pos.y == -2) shouldEqual true

   result.exists(r => r.pos.x == 8 && r.pos.y == 17) shouldEqual false
   result.exists(r => r.pos.x == 8 && r.pos.y == -3) shouldEqual false

   result.exists(r => r.pos.x == -1 && r.pos.y == 7) shouldEqual true
   result.exists(r => r.pos.x == 16 && r.pos.y == 7) shouldEqual true

   result.exists(r => r.pos.x == 7 && r.pos.y == 16) shouldEqual false
   result.exists(r => r.pos.x == 7 && r.pos.y == -2) shouldEqual false

   result.count(_.pos.x == 8) shouldEqual 18 // this has the sensor, so counts one less
   result.count(_.pos.x == 7) shouldEqual 17
   result.count(_.pos.x == 9) shouldEqual 17
   result.count(_.pos.x == 6) shouldEqual 15
   result.count(_.pos.x == 10) shouldEqual 15
   result.count(_.pos.x == 5) shouldEqual 13
   result.count(_.pos.x == 11) shouldEqual 13
   result.count(_.pos.x == 4) shouldEqual 11
   result.count(_.pos.x == 12) shouldEqual 11
   result.count(_.pos.x == 3) shouldEqual 9
   result.count(_.pos.x == 13) shouldEqual 9
   result.count(_.pos.x == 2) shouldEqual 6 // this has the beacon, so counts one less
   result.count(_.pos.x == 14) shouldEqual 7
   result.count(_.pos.x == 1) shouldEqual 5
   result.count(_.pos.x == 15) shouldEqual 5
   result.count(_.pos.x == 0) shouldEqual 3
   result.count(_.pos.x == 16) shouldEqual 3


   result.toList.length shouldEqual 179
 }

  "SensorBeaconPair.fromString" should "be able to parse from string" in {
    val result = SensorBeaconPair.fromString("Sensor at x=3, y=18: closest beacon is at x=-2, y=15").get

    result.sensor.pos shouldEqual Coordinate(3, 18)
    result.closestBeacon.pos shouldEqual Coordinate(-2, 15)
  }

  "BeaconZone#beaconExclusionZones" should "work correctly for the sample input" in {
    val args = FileUtils.readFile("day15/sample")

    val beaconZone: BeaconZone = BeaconZone(args.flatMap(SensorBeaconPair.fromString))

    beaconZone.print

    beaconZone.beaconExclusionPoints.count(_.pos.y == 10) shouldEqual 26
  }
}
