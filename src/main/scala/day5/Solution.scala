package day5

import common.AdventProblemSolution

import scala.annotation.tailrec

object Solution extends AdventProblemSolution[String, String] {

  override def part1(args: List[String]): String = {

    val (crateLines, moveLines) = splitInputArgs(args, List.empty)

    val crateStacks = CrateStack.constructCrateStacks(crateLines)
    val moves: List[MoveAction] = moveLines.flatMap(MoveAction.constructFromString)

    val result = CrateStackUtils.makeMoves(moves, crateStacks)
    CrateStackUtils.topCrates(result._2)
  }

  override def part2(args: List[String]): String = {

    val (crateLines, moveLines) = splitInputArgs(args, List.empty)

    val crateStacks = CrateStack.constructCrateStacks(crateLines)
    val moves: List[MoveAction] = moveLines.flatMap(MoveAction.constructFromString)

    val result = CrateStackUtils.makeMoves(moves, crateStacks, with9001 = true)

    CrateStackUtils.topCrates(result._2)
  }

 // This tail recursive functions helps split the input args into two parts where one is for crates and the other is for moves
  @tailrec
  private def splitInputArgs(args: List[String], result: (List[String])): (List[String], List[String]) = args match {
    case Nil => (result, List.empty)
    case head :: tail if (head.isEmpty) => (result, tail)
    case head :: tail => splitInputArgs(tail, (result :+ head))
  }
}
