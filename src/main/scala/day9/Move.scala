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

    str.split(" ").toList match {
      case l if l.length != 2 => List.empty
      case head1 :: head2 :: _ if head1 == "R" => buildMoves[MoveRight](head2, MoveRight())
      case head1 :: head2 :: _ if head1 == "U" => buildMoves[MoveUp](head2, MoveUp())
      case head1 :: head2 :: _ if head1 == "D" => buildMoves[MoveDown](head2, MoveDown())
      case head1 :: head2 :: _ if head1 == "L" => buildMoves[MoveLeft](head2, MoveLeft())
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
