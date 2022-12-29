package solutions

import common.FileUtils
import day18.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay18Spec extends AnyFlatSpec with Matchers {
  "Day 18: Part one (sample)" should "equal with the expected exposed sides of the structure" in {
    val args = FileUtils.readFile("day18/sample")

    Solution.part1(args) shouldEqual 64
  }

  "Day 18: Part one" should "equal with the expected exposed sides of the structure" in {
    val args = FileUtils.readFile("day18/input")

    Solution.part1(args) shouldEqual 4628
  }
}
