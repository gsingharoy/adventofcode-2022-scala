package day13

import common.{AdventProblemSolution, ListUtils}

object Solution extends AdventProblemSolution[Int, Int] {
  override def part1(args: List[String]): Int = {
    val inputTuples = ListUtils.zipListByPivotValue(args, "").flatMap(ListUtils.transformToTuplePair)
    indicesInRightOrder(inputTuples).sum
  }

  override def part2(args: List[String]): Int = ???

  private def indicesInRightOrder(packets: List[(String, String)]): List[Int] =
    packets.zipWithIndex.flatMap {
      case ((l, r), i) => {
        if (ListMatcher.compareStrLists(l, r)) Some(i + 1) else None
      }
    }
}
