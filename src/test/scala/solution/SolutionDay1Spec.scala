package solution

import common.FileUtils
import day1.{Elf, ElfCalorieFinder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay1Spec extends AnyFlatSpec with Matchers {

  "Solution Day 1: Part one" should "be able to find the maximum calorific value of the elves participating from the input of the question " in {
    val rawCalories = FileUtils.readFile("day1/input")
    val elves: List[Elf] = Elf.constructElves(rawCalories)

    ElfCalorieFinder.findElfWithMaxCalories(elves).get.totalCalories shouldBe 68787
  }

  "Solution Day 2: Part 2s" should "be able to find the maximum calorific value of the top 3 elves as part of the bonus question " in {
    val rawCalories = FileUtils.readFile("day1/input")
    val elves: List[Elf] = Elf.constructElves(rawCalories)

    ElfCalorieFinder.findTopElvesWithCalorificValues(elves, 3).map(_.totalCalories).sum shouldBe 198041
  }
}
