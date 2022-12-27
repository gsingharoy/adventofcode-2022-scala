package day15

import scala.annotation.tailrec

case class BeaconZone(beaconPairs: List[SensorBeaconPair]){

   def beaconExclusionRanges(y: Int): List[YCoordinateRange] = {
     val allRanges: List[YCoordinateRange] = CoordinateRangeUtils.splitWithPoints(
       ranges = rangesWithoutDistressSignal(y),
       points = beaconPairs.map(_.sensor.pos) ++ beaconPairs.map(_.closestBeacon.pos)
     )
     CoordinateRangeUtils.mergeRanges(allRanges)
   }

  def rangesWithoutDistressSignal(y: Int): List[YCoordinateRange] =
    CoordinateRangeUtils.mergeRanges(beaconPairs.flatMap(_.rangeWithoutDistressSignal(y)))

}

case class SensorBeaconPair(sensor: Sensor, closestBeacon: Beacon) {

  private lazy val maxPossibleDistanceForNoBeacons: Int = sensor.pos.manhattanDistance(closestBeacon.pos)

  def beaconExclusionRanges(y: Int): List[YCoordinateRange] =
    rangeWithoutDistressSignal(y)
      .map(_.split(sensor.pos).flatMap(_.split(closestBeacon.pos)).sorted)
      .getOrElse(List.empty)


  def rangeWithoutDistressSignal(y: Int): Option[YCoordinateRange] =
    if ((y - sensor.pos.y).abs > maxPossibleDistanceForNoBeacons)
      None // not within the expected manhattan distance
    else {
      val xLength: Int = maxPossibleDistanceForNoBeacons - (y - sensor.pos.y).abs
      Some(YCoordinateRange(y = y, xStart = sensor.pos.x - xLength, xEnd = sensor.pos.x + xLength))
    }

}

object SensorBeaconPair {
  def fromString(str: String): Option[SensorBeaconPair] = str match {
    case s"Sensor at x=${sx1}, y=${sy1}: closest beacon is at x=${sx2}, y=${sy2}" =>
      ((sx1.toIntOption, sy1.toIntOption), (sx2.toIntOption, sy2.toIntOption)) match {
        case ((Some(ix1),Some(iy1) ), (Some(ix2), Some(iy2))) => {
          val beacon = Beacon(Coordinate(x = ix2, y = iy2))
          val sensor = Sensor(Coordinate(x = ix1, y = iy1))
          Some(SensorBeaconPair(sensor, beacon))
        }
        case _ => None
      }
    case _ => None
  }
}
