package day9


trait Knot[T] {
  def pos: Position

  def move(move: Move): T

  protected def newPosition(move: Move): Position = move match {
    case _: MoveUp => pos.copy(y = pos.y + 1)
    case _: MoveDown => pos.copy(y = pos.y - 1)
    case _: MoveLeft => pos.copy(x = pos.x - 1)
    case _: MoveRight => pos.copy(x = pos.x + 1)
    case _: MoveDiagonalRightUp => pos.copy(x = pos.x + 1, y = pos.y + 1)
    case _: MoveDiagonalRightDown => pos.copy(x = pos.x + 1, y = pos.y - 1)
    case _: MoveDiagonalLeftDown => pos.copy(x = pos.x - 1, y = pos.y - 1)
    case _: MoveDiagonalLeftUp => pos.copy(x = pos.x - 1, y = pos.y + 1)
    case _ => pos // returns the same position
  }
}

case class Position(x: Int, y: Int)

case class Head(pos: Position) extends Knot[Head] {
  override def move(move: Move): Head = this.copy(newPosition(move))
}

object Head {

  lazy val StartPosition: Head = Head(Position(0, 0))
}

case class Tail(pos: Position) extends Knot[Tail] {
  override def move(move: Move): Tail = this.copy(newPosition(move))
}

object Tail {

  lazy val StartPosition: Tail = Tail(Position(0, 0))
}

case class RopeBridge (moves: List[MoveHistory]) {

  lazy val head: Head = moves.lastOption.map(_.head).getOrElse(Head.StartPosition)

  lazy val tail: Tail = moves.lastOption.map(_.tail).getOrElse(Tail.StartPosition)

  lazy val uniqueTailPositions: Int = moves.map(_.tail.pos).distinct.length
}

