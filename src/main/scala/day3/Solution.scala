package day3

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int] {
  override def part1(args: List[String]): Int = {
    val ruckSacks: List[Rucksack] = args.flatMap(Rucksack.constructFromString)

    // return total score
    ruckSacks.map(r => r.duplicatedItems.map(RucksackUtils.itemScore).sum).sum
  }

  override def part2(args: List[String]): Int = {
    val ruckSacks: List[Rucksack] = args.flatMap(Rucksack.constructFromString)

    val rucksackGroups = RucksackGroup.constructFromRucksacks(ruckSacks)

    // return total score
    rucksackGroups.flatMap(_.badge).map(RucksackUtils.itemScore).sum
  }
}
