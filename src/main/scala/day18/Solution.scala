package day18

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int] {

  override def part1(args: List[String]): Int =
    CubeStructure.fromStrings(args).exposedSides

  override def part2(args: List[String]): Int = ???
}
