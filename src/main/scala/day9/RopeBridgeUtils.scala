package day9

import scala.annotation.tailrec

/**
 * Contains helper methods to make movements to the knot of the ropes
 */
object RopeBridgeUtils {

  /**
   * Checks if the tail is adjacent to the head. If that is not the case case then true is returned.
   * @param tail
   * @param head
   * @return true if the tail is needed to make a move.
   */
  def isTailNeededToMove(tail: Tail, head: Head): Boolean =
    ((tail.pos.x - head.pos.x).abs, (tail.pos.y - head.pos.y).abs) match {
      case (x, y) if (x <= 1 && y <= 1) => false // the tail is adjacent and there is no need to make a move
      case _ => true
    }


  /**
   * This function adjusts the tail so it is adjacent to the head. If it is already adjacent to the head then it does not
   * make a move and returns the same position of the tail
   *
   * @param tail
   * @param head
   * @return
   */
  def adjustTail(tail: Tail, head: Head): Tail = {
    if (!isTailNeededToMove(tail, head))
      return tail // there is no need to make a move and the same tail would remain as it is still adjacent to the head.
    (head.pos.x - tail.pos.x, head.pos.y - tail.pos.y) match {
      case (x, y) if (x <= -2 && y == 0) => tail.move(MoveLeft()) // tail is on the left
      case (x, y) if (x >= 2 && y == 0) => tail.move(MoveRight()) // tail is on the right
      case (x, y) if (x == 0 && y <= -2) => tail.move(MoveDown()) // tail is on two spaces up
      case (x, y) if (x == 0 && y >= 2) => tail.move(MoveUp()) // tail is on two spaces down
      case (x, y) if (x <= -1 && y <= -1) => tail.move(MoveDiagonalLeftDown()) // tail is top right diagonally
      case (x, y) if (x <= -1 && y >= 1) => tail.move(MoveDiagonalLeftUp()) // tail is bottom right diagonally
      case (x, y) if (x >= 1 && y >= 1) => tail.move(MoveDiagonalRightUp()) // tail is bottom left diagonally
      case (x, y) if (x >= 1 && y <= -1) => tail.move(MoveDiagonalRightDown()) // tail is top left diagonally
      case _ => tail //fail safe to not move at all.
    }
  }

  def makeMoves(allMoves: List[Move],
                startHead: Head = Head.StartPosition,
                startTail: Tail = Tail.StartPosition): RopeBridge = {

    @tailrec
    def f(moves: List[Move],
          head: Head,
          tail: Tail,
          result: List[MoveHistory]): List[MoveHistory] = moves match {
      case Nil => result.reverse // the result is reversed to make the algorithm efficient as it is cheaper to append item to the start of a list
      case m :: t => {
        val newHead = head.move(m)
        val newTail = adjustTail(tail, newHead)
        f(t, newHead, newTail, MoveHistory(newHead, newTail) :: result)
      }
    }

    RopeBridge(f(allMoves, startHead, startTail, List(MoveHistory(Head.StartPosition, Tail.StartPosition))))
  }


}
