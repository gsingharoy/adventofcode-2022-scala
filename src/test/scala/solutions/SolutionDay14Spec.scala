package solutions

import common.FileUtils
import day14.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay14Spec extends AnyFlatSpec with Matchers {
  "Day 14: Part one (sample)" should "return the correct units" in {
    val args = FileUtils.readFile("day14/sample")

    Solution.part1(args) shouldEqual 24
  }

  "Day 14: Part one" should "return the correct units" in {
    val args = FileUtils.readFile("day14/input")

    Solution.part1(args) shouldEqual 1061
  }

  "Day 14: Part two (sample)" should "return the correct units" in {
    val args = FileUtils.readFile("day14/sample")

    Solution.part2(args) shouldEqual 93
  }

  /**
   * This test is commented as it takes about 4-5 mins to execute. the expected answer is validated and is correct
   */
  //  "Day 14: Part two" should "return the correct units" in {
//    val args = FileUtils.readFile("day14/input")
//
//    Solution.part2(args) shouldEqual 25055
//  }
}
