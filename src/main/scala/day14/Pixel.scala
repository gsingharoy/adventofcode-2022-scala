package day14

import scala.annotation.tailrec


trait Pixel[T] extends Ordered[Pixel[T]] {
  def pos: Position
  def char: Char

  def compare(that: Pixel[T]): Int = this.pos compare that.pos
}

case class Position(x: Int, y: Int) extends Ordered[Position] {

  import scala.math.Ordered.orderingToOrdered
  /**
   * Function to return positions of other points from the current point to draw a straight line to them
   *
   * @param endPos
   * @return
   */
  def drawLine(endPos: Position): List[Position] = (this.x - endPos.x,this.y - endPos.y) match {
    case (0, y) if y > 0 => Range.inclusive(endPos.y, this.y -1).toList.map(Position(this.x, _))
    case (0, y) if y < 0 => Range.inclusive(this.y + 1, endPos.y).toList.map(Position(this.x, _))
    case (x, 0) if x > 0 => Range.inclusive(endPos.x, this.x-1).toList.map(Position(_, this.y))
    case (x, 0) if x < 0 => Range.inclusive(this.x + 1, endPos.x).toList.map(Position(_, this.y))
    case _ => List.empty
  }

  override def compare(that: Position): Int = (this.x, this.y) compare (that.x, that.y)

  lazy val posAbove: Position = this.copy(y = this.y -1)
  lazy val posDiagonallyLeft: Position = posBelow.copy(x = this.x - 1)

  lazy val posDiagonallyRight: Position = posBelow.copy(x = this.x + 1)

  lazy val posLeft: Position = this.copy(x = this.x - 1)
  lazy val posRight: Position = this.copy(x = this.x + 1)

  lazy val posBelow: Position = this.copy(y = this.y + 1)
}

object Position {

  def fromStr(str: String): Option[Position] = str match {
    case s"${x},${y}" => (x.toIntOption, y.toIntOption) match {
      case (Some(ix), Some(iy)) => Some(Position(x = ix, y = iy))
      case _ => None
    }
    case _ => None
  }
}

case class Brick(pos: Position) extends Pixel[Brick] {
  val char: Char = Brick.char
}


object Brick {

  val char: Char = '#'
  def fromString(str: String): List[Brick] = {

    @tailrec
    def f(strs: List[String],
          lastPos: Option[Brick],
          result: List[Brick]): List[Brick] = (strs, lastPos) match {
      case (Nil, _) => result
      case (head :: tail, None) => {
        // create the first brick
        val newBrick = Position.fromStr(head).map(Brick(_))
        f(tail, newBrick, newBrick.toList)
      }
      case (head :: tail, Some(brick)) =>
        Position.fromStr(head) match {
          case Some(p) => f(tail, Some(Brick(p)), result ++ brick.pos.drawLine(p).map(Brick(_)))
          case None => f(tail, lastPos, result)
        }
    }

    f(str.split("->").toList.map(_.trim), None, List.empty)
  }
}

case class Sand(pos: Position) extends Pixel[Sand] {
  val char: Char = 'o'

}
