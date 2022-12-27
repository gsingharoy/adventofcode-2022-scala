package day15

case class YCoordinateRange(y: Int, xStart: Int, xEnd: Int) extends Ordered[YCoordinateRange] {

  /**
   * Attempts to split the range with the integer coordinate
   * @param coordinate
   * @return
   */
  def split(coordinate: Coordinate): List[YCoordinateRange] = (coordinate.x, coordinate.y) match {
    case (x, y) if y != this.y || x < this.xStart || x > this.xEnd => List(this) // the coordinate is not within the range
    case (x, _) if x == this.xStart => List(this.copy(xStart = this.xStart + 1))
    case (x, _) if x == this.xEnd => List(this.copy(xEnd = this.xEnd - 1))
    case (x, _) => List(this.copy(xEnd = x -1), this.copy(xStart = x +1)) // Split in two lists
  }

  /**
   * Build a list of ranges where they are attempted to be merged together. Returns a None value if no merges are possible
   *
   * @param that
   * @return
   */
  def merge(that: YCoordinateRange): Option[YCoordinateRange] = ((this.xStart, this.xEnd), (that.xStart, that.xEnd)) match {
    case ((x1, x2), (x3, x4)) if x1 >= x3 && x2<= x4 => Some(that) // curr range is completely within the that
    case ((x1, x2), (x3, x4)) if x3 >= x1 && x4<= x2 => Some(this) // that range is completely within the curr one
    case ((x1, x2), (x3, x4)) if !Range.inclusive(x3, x4).contains(x1) && Range.inclusive(x3, x4).contains(x2) =>
      Some(this.copy(xEnd = x4))
    case ((x1, x2), (x3, x4)) if Range.inclusive(x3, x4).contains(x1) && !Range.inclusive(x3, x4).contains(x2) =>
      Some(that.copy(xEnd = x2))
    case ((x1, x2), (x3, x4)) if !Range.inclusive(x1, x2).contains(x3) && Range.inclusive(x1, x2).contains(x4) =>
      Some(this.copy(xStart = x3))
    case ((x1, x2), (x3, x4)) if Range.inclusive(x1, x2).contains(x3) && !Range.inclusive(x1, x2).contains(x4) =>
      Some(this.copy(xEnd = x4))
    case  ((_, x2), (x3, x4)) if x3 - x2 == 1 => Some(this.copy(xEnd = x4))
    case ((x1, x2), (_, x4)) if x1 - x4 == 1 => Some(that.copy(xEnd = x2))
    case _ => None // lazy programming. Could be a failure type here
  }

  override def compare(that: YCoordinateRange): Int = this.xStart - that.xStart

  /**
   * Total existing coordinates
   */
  lazy val totalCoordinates: Int = (xEnd - xStart).abs + 1

}

case class Coordinate(x: Int, y: Int) extends Ordered[Coordinate] {
  import scala.math.Ordered.orderingToOrdered

  override def compare(that: Coordinate): Int =   (this.x, this.y) compare (that.x, that.y)

  def manhattanDistance(that: Coordinate): Int = (this.x - that.x).abs + (this.y - that.y).abs

  def matches(that: Coordinate): Boolean = this.x == that.x && this.y == that.y
}
