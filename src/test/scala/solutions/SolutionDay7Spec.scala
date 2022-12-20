package solutions

import common.FileUtils
import day7.{CommandUtils, DataFile, Solution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay7Spec extends AnyFlatSpec with Matchers {

  "Day 7: Part one" should "be able to find the directories with sizes more than 100k and sum them up" in {

    val args = FileUtils.readFile("day7/input")

    Solution.part1(args) shouldEqual 1453349
  }

  "Day 7: part two" should "be able to find the smallest file to delete and free up the space" in {
    val args = FileUtils.readFile("day7/input")

    Solution.part2(args) shouldEqual 2948823
  }

}
