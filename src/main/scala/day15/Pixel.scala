package day15


trait Pixel[T] extends Ordered[Pixel[T]] {
  def pos: Coordinate
  def char: Char


  def compare(that: Pixel[T]): Int = this.pos compare that.pos
}

case class Sensor(pos: Coordinate) extends Pixel[Sensor] {
  lazy val char: Char = Sensor.char
}

object Sensor {
  val char: Char = 'S'
}

case class Beacon(pos: Coordinate) extends Pixel[Coordinate] {
  lazy val char: Char = Beacon.char
}

object Beacon {
  val char: Char = 'B'
}

case class BeaconExclusionPoint(pos: Coordinate) extends Pixel[BeaconExclusionPoint] {
  lazy val char: Char = BeaconExclusionPoint.char
}

object BeaconExclusionPoint {
  val char: Char = '#'
}





