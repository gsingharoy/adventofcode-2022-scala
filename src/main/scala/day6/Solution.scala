package day6

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Option[Int], Option[Int]] {

  override def part1(args: List[String]): Option[Int] =
    args.headOption.flatMap(Device(_, 4).startOfMarker)

  override def part2(args: List[String]): Option[Int] =
    args.headOption.flatMap(Device(_, 14).startOfMarker)
}
