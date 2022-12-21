package day10

import scala.annotation.tailrec

object CathodeRayUtils {

  def makeOperations(operations: List[Operation], startX: Int = 1): (Int, List[CycleHistory]) = {

    @tailrec
    def fOp(ops: List[Operation], x: Int, history: List[CycleHistory]): (Int, List[CycleHistory])  = ops match {
      case Nil => (x, history)
      case head :: tail => {
        val (newX, cycleHistories) = head.exec(totalCycles = history.length, x = x)
        fOp(tail, newX, history ++ cycleHistories)
      }
    }

    fOp(operations, startX, List.empty)
  }

  def findSignalStrength(duringCycle: Int, cycleHistories: List[CycleHistory]): Int =
    cycleHistories.find(_.cycle == duringCycle).map(_.startX * duringCycle).getOrElse(0)

}
