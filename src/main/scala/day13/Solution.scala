package day13

import common.{AdventProblemSolution, ListUtils}

object Solution extends AdventProblemSolution[Int, Int] {
  override def part1(args: List[String]): Int = {
    val inputTuples = ListUtils.zipListByPivotValue(args, "").flatMap(ListUtils.transformToTuplePair)
    indicesInRightOrder(inputTuples).sum
  }

  override def part2(args: List[String]): Int = {
    val divider1: String = "[[2]]"
    val divider2: String = "[[6]]"

    val allLists: List[String] = ListUtils.zipListByPivotValue(args, "").flatten ++
      List(divider1, divider2)

    // Sorting the packets so they all are in order
    val sortedPackets: List[String] = allLists.sortWith(ListMatcher.compareStrLists)
    val divider1Index: Int = sortedPackets.indexOf(divider1) + 1
    val divider2Index: Int = sortedPackets.indexOf(divider2) + 1

    println(s"Found divider 1: ${divider1} at index ${divider1Index}")
    println(s"Found divider 1: ${divider2} at index ${divider2Index}")

    divider1Index * divider2Index
  }

  private def indicesInRightOrder(packets: List[(String, String)]): List[Int] =
    packets.zipWithIndex.flatMap {
      case ((l, r), i) => {
        if (ListMatcher.compareStrLists(l, r)) Some(i + 1) else None
      }
    }
}
