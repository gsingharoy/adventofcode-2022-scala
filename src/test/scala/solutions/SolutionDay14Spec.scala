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
}
