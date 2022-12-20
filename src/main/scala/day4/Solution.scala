package day4

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int]{

  override def part1(args: List[String]): Int = {
    val assignments: List[Assignment] = args.flatMap(Assignment.constructFromString)

    assignments.count(_.hasCompleteOverlap)
  }

  override def part2(args: List[String]): Int = {
    val assignments: List[Assignment] = args.flatMap(Assignment.constructFromString)

    assignments.count(_.hasPartialOverlap)
  }
}
