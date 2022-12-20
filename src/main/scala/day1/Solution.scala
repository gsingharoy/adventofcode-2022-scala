package day1

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int] {

  override def part1(args: List[String]): Int = {
    val elves: List[Elf] = Elf.constructElves(args)
    ElfCalorieFinder.findElfWithMaxCalories(elves).get.totalCalories
  }

  override def part2(args: List[String]): Int = {
    val elves: List[Elf] = Elf.constructElves(args)

    ElfCalorieFinder.findTopElvesWithCalorificValues(elves, 3).map(_.totalCalories).sum
  }
}
