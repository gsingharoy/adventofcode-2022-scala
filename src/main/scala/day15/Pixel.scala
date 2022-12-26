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


case class Coordinate(x: Int, y: Int) extends Ordered[Coordinate] {
  import scala.math.Ordered.orderingToOrdered

  override def compare(that: Coordinate): Int =   (this.x, this.y) compare (that.x, that.y)

  def manhattanDistance(that: Coordinate): Int = (this.x - that.x).abs + (this.y - that.y).abs

  def matches(that: Coordinate): Boolean = this.x == that.x && this.y == that.y
}
