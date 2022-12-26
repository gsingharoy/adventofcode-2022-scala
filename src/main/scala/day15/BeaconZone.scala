package day15

import scala.annotation.tailrec

case class BeaconZone(beaconPairs: List[SensorBeaconPair]){

  lazy val beaconExclusionPoints: List[BeaconExclusionPoint] = {
    val allSensorAndBeaconCoordinates: List[Coordinate] = (beaconPairs.map(_.sensor.pos) ++
      beaconPairs.map(_.closestBeacon.pos))

    beaconPairs.flatMap(_.beaconExclusionPoints)
      .distinct
      .filter(p => !allSensorAndBeaconCoordinates.contains(p.pos))
  }

  private lazy val allPixels: List[Pixel[_]] = ((beaconPairs.map(_.sensor) ++
    beaconPairs.map(_.closestBeacon)).sorted ++ beaconExclusionPoints).sortWith( (a1, a2) => a1.pos.compare(a2.pos) < 0)

  private lazy val topLimit: Int = allPixels.map(_.pos.y).sorted.headOption.getOrElse(0)
  private lazy val bottomLimit: Int = allPixels.map(_.pos.y).sortWith(_ > _).headOption.getOrElse(0)

  private lazy val leftLimit: Int = allPixels.map(_.pos.x).sorted.headOption.getOrElse(0)
  private lazy val rightLimit: Int = allPixels.map(_.pos.x).sortWith(_ > _).headOption.getOrElse(0)

  /**
   * Returns a printable String format
   * @return
   */
  def mkstring: List[String] = Range.inclusive(topLimit, bottomLimit).toList.map(currY => {

    Range.inclusive(leftLimit, rightLimit).toList.map(currX => {
      val currC = Coordinate(x = currX, y = currY)
      allPixels.find(_.pos.matches(currC)).map(_.char).getOrElse('.')
    }).mkString
  }
  )

  def print: Unit = mkstring.foreach(println)
}

case class SensorBeaconPair(sensor: Sensor, closestBeacon: Beacon) {

  private lazy val maxPossibleDistanceForNoBeacons: Int = sensor.pos.manhattanDistance(closestBeacon.pos)

  lazy val beaconExclusionPoints: List[BeaconExclusionPoint] = {
  println(s"Starting to find beacon exlcusion points for Sensor[${this.sensor.pos.x},${this.sensor.pos.y}] && Beacon[${this.closestBeacon.pos.x}, ${this.closestBeacon.pos.y}]")
    @tailrec
    def horizontal(curr: Coordinate,
                   result: List[BeaconExclusionPoint] = List.empty): List[BeaconExclusionPoint] =
      (curr.x == sensor.pos.x, curr.manhattanDistance(sensor.pos) <= maxPossibleDistanceForNoBeacons) match {
        case (_, false) => result // this point is farther than the locked beacon
        case (true, true) => { // this point is on the same x axis. so it would return only one possible point
          val nextPos = curr.copy(x = curr.x - 1)
          val newResult: List[BeaconExclusionPoint] = result ++ List(curr).map(BeaconExclusionPoint(_))
          horizontal(nextPos, newResult)
        }
        case (false, true) => { // this point is NOT on the same x axis. so it would return only two possible points
          val nextPos = curr.copy(x = curr.x - 1)
          val newResult: List[BeaconExclusionPoint] = result ++
            List(curr, curr.copy(x = (curr.x - sensor.pos.x).abs + sensor.pos.x)).map(BeaconExclusionPoint(_))
          horizontal(nextPos, newResult)
        }
      }


    Range.inclusive(sensor.pos.y - maxPossibleDistanceForNoBeacons, sensor.pos.y + maxPossibleDistanceForNoBeacons)
      .toList
      .flatMap(currY => horizontal(sensor.pos.copy(y = currY)))
      .filter(b => !b.pos.matches(sensor.pos) && !b.pos.matches(closestBeacon.pos)) // filter out the positions of the beacon and the sensor
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
