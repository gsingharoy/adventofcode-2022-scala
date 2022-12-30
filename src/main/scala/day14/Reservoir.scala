package day14

import scala.annotation.tailrec

case class Reservoir(
    pixels: List[Pixel[_]],
    sandEntryPoint: Position = Position(500, 0),
    withFixedBottom: Boolean = false
) {

  private lazy val leftLimit: Int = pixels.map(_.pos.x).sorted.headOption.getOrElse(0)

  private lazy val rightLimit: Int = pixels.map(_.pos.x).sortWith(_ > _).headOption.getOrElse(1)

  private lazy val bottomBrickLimit: Int = pixels
    .filter({
      case _: Brick => true
      case _        => false
    })
    .map(_.pos.y)
    .sortWith(_ > _)
    .headOption
    .getOrElse(0)

  private lazy val bottomLimit: Int = pixels.map(_.pos.y).sortWith(_ > _).headOption.getOrElse(0)

  private lazy val topLimit: Int = 0

  lazy val mkString: List[String] = Range
    .inclusive(topLimit, bottomLimit)
    .toList
    .map(y =>
      Range
        .inclusive(leftLimit, rightLimit)
        .toList
        .map(x =>
          pixels.find(p => p.pos.x == x && p.pos.y == y) match {
            case Some(p) => p.char
            case None    => '.'
          }
        )
        .mkString
    ) ++ {
    if (withFixedBottom) {
      List(
        Range.inclusive(leftLimit, rightLimit).toList.map(_ => Brick.char).mkString
      )
    } else List.empty
  }

  private lazy val nextPositionForSand: Option[Position] = {

    @tailrec
    def findStopPoint(curr: Position): Option[Position] = {
      if (
        findPixel(
          sandEntryPoint
        ).isDefined || // the entry point is full. We should not attempt to pour more sand
        (!withFixedBottom && posInAbyss(curr)) || // condition for abyss
        curr.y >= bottomBrickLimit + 2            // condition for the floor
      ) return None
      if (withFixedBottom && curr.y == this.bottomBrickLimit + 1)
        return Some(curr) // reached the fixed bottom
      (
        findPixel(curr.posBelow),
        findPixel(curr.posDiagonallyLeft),
        findPixel(curr.posDiagonallyRight)
      ) match {
        case (None, _, _) => findStopPoint(curr.posBelow) // Bottom space is empty
        case (Some(_), None, _) =>
          findStopPoint(curr.posDiagonallyLeft) // Move diagonally to the left
        case (Some(_), Some(_), None) =>
          findStopPoint(curr.posDiagonallyRight) // Move diagonally to the right
        case _ => Some(curr) // Sand stops here
      }
    }

    findStopPoint(sandEntryPoint)
  }

  def newReservoirWithASandUnit: Option[Reservoir] =
    nextPositionForSand.map(p => this.copy(pixels = Sand(p) :: this.pixels))

  lazy val isFullOfSand: Boolean = findPixel(sandEntryPoint).isDefined

  lazy val totalSandUnits: Int = pixels.count {
    case _: Sand => true
    case _       => false
  }

  private def findPixel(pos: Position): Option[Pixel[_]] =
    pixels.find(p => p.pos.x == pos.x && p.pos.y == pos.y)

  private def posInAbyss(pos: Position): Boolean =
    !(pos.x >= leftLimit && pos.x <= rightLimit && pos.y >= topLimit && pos.y <= bottomLimit)
}

object Reservoir {

  def fromStrings(strings: List[String]): Reservoir = Reservoir(
    strings.flatMap(Brick.fromString).sorted
  )
}
