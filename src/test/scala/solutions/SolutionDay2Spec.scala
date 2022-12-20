package solutions

import common.FileUtils
import day2.{GameAction, PlayConfig, ResultConfig, Solution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay2Spec extends  AnyFlatSpec with Matchers{

  "Solution 2: Part 1" should "be able to find the correct score based on the available strategy" in {
    val args = FileUtils.readFile("day2/input")

    Solution.part1(args) shouldEqual 15572
  }

  "Solution 2: Part 2" should "be able to find the correct score based on the available strategy for the bonus question" in {
    val args = FileUtils.readFile("day2/input")

    Solution.part2(args) shouldEqual 16098
  }

}
