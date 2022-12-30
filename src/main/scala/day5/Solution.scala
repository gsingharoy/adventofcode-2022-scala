package day5

import common.{AdventProblemSolution, ListUtils}

object Solution extends AdventProblemSolution[String, String] {

  override def part1(args: List[String]): String = {

    val (crateLines, moveLines) = splitInputArgs(args)

    val crateStacks             = CrateStack.constructCrateStacks(crateLines)
    val moves: List[MoveAction] = moveLines.flatMap(MoveAction.constructFromString)

    val result = CrateStackUtils.makeMoves(moves, crateStacks)
    CrateStackUtils.topCrates(result._2)
  }

  override def part2(args: List[String]): String = {

    val (crateLines, moveLines) = splitInputArgs(args)

    val crateStacks             = CrateStack.constructCrateStacks(crateLines)
    val moves: List[MoveAction] = moveLines.flatMap(MoveAction.constructFromString)

    val result = CrateStackUtils.makeMoves(moves, crateStacks, with9001 = true)

    CrateStackUtils.topCrates(result._2)
  }

  // This tail recursive functions helps split the input args into two parts where one is for crates and the other is for moves
  private def splitInputArgs(args: List[String]): (List[String], List[String]) =
    ListUtils.zipListByPivotValue(args, "") match {
      case l if l.length != 2 => {
        println("Ill formed arguments detected ...")
        (List.empty, List.empty)
      }
      case cl :: ml :: _ => (cl, ml)
    }
}
