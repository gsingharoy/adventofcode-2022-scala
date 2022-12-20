package solutions

import common.FileUtils
import day1.{Elf, ElfCalorieFinder, Solution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay1Spec extends AnyFlatSpec with Matchers {

  "Solution Day 1: Part one" should "be able to find the maximum calorific value of the elves participating from the input of the question " in {
    val args = FileUtils.readFile("day1/input")
    Solution.part1(args) shouldBe 68787
  }

  "Solution Day 2: Part 2s" should "be able to find the maximum calorific value of the top 3 elves as part of the bonus question " in {
    val args = FileUtils.readFile("day1/input")

    Solution.part2(args) shouldBe 198041
  }
}
