package solutions

import common.FileUtils
import day3.{Rucksack, RucksackGroup, RucksackUtils, Solution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay3Spec extends AnyFlatSpec with Matchers {

  "Solution Day 3: Part one" should "be able find the correct total of the scores" in {
    val args = FileUtils.readFile("day3/input")

    Solution.part1(args) shouldEqual 7863
  }

  "Solution Day 3: Part two" should "be able find the correct total of all the badges" in {
    val args = FileUtils.readFile("day3/input")

    Solution.part2(args) shouldEqual 2488
  }
}
