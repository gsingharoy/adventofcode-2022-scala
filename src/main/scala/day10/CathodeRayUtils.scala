package day10

import scala.annotation.tailrec

object CathodeRayUtils {

  def makeOperations(operations: List[Operation], startX: Int = 1): (Int, List[CycleHistory]) = {

    @tailrec
    def fOp(ops: List[Operation], x: Int, history: List[CycleHistory]): (Int, List[CycleHistory]) =
      ops match {
        case Nil => (x, history)
        case head :: tail => {
          val (newX, cycleHistories) = head.exec(totalCycles = history.length, x = x)
          fOp(tail, newX, history ++ cycleHistories)
        }
      }

    fOp(operations, startX, List.empty)
  }

  def buildPixels(cycles: List[CycleHistory]): List[String] = {

    @tailrec
    def draw(c: List[CycleHistory], result: List[Char]): List[Char] = c match {
      case Nil => result.reverse
      case h :: tail => {
        val currPos = h.cycle - 1
        val pixelToDraw: Char = {
          // this adjusts to the particular vertical row based on cycles
          val x: Int = h.startX + (40 * (currPos / 40))
          if ((currPos == x) || (currPos == x - 1) || (currPos == x + 1)) {
            // sprite is overlapping the position
            '#'
          } else {
            '.'
          }
        }
        draw(tail, pixelToDraw :: result)
      }
    }

    draw(cycles, List.empty)
      .grouped(40)
      .toList
      .map(_.mkString)
  }

  def findSignalStrength(duringCycle: Int, cycleHistories: List[CycleHistory]): Int =
    cycleHistories.find(_.cycle == duringCycle).map(_.startX * duringCycle).getOrElse(0)

}
