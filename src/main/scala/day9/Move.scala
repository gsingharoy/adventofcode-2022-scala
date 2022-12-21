package day9

trait Move

object Move {

  def fromStrings(strings: List[String]): List[Move] = strings.flatMap(fromString)
  def fromString(str: String): List[Move] = {

    def buildMoves[T <: Move](units: String, move: T): List[T] =
      units
        .toIntOption
        .map(i=> Range.inclusive(1, i).toList.map(_ => move))
        .getOrElse(List[T]())

    str match {
      case s"R ${u}" => buildMoves[MoveRight](u, MoveRight())
      case s"U ${u}" => buildMoves[MoveUp](u, MoveUp())
      case s"D ${u}" => buildMoves[MoveDown](u, MoveDown())
      case s"L ${u}" => buildMoves[MoveLeft](u, MoveLeft())
      case _ => List.empty
    }
  }


}

case class MoveUp() extends Move

case class MoveDown() extends Move

case class MoveLeft() extends Move

case class MoveRight() extends Move

case class MoveDiagonalRightUp() extends Move

case class MoveDiagonalRightDown() extends Move

case class MoveDiagonalLeftUp() extends Move

case class MoveDiagonalLeftDown() extends Move


case class MoveHistory(head: Head, tails: List[Tail])
