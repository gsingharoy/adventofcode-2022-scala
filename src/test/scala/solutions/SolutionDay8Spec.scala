package solutions

import common.FileUtils
import day8.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay8Spec extends AnyFlatSpec with Matchers {

  "Day 8: Part one" should "be able to count the trees which are visible to one of the edges" in {
    val args = FileUtils.readFile("day8/input")

    Solution.part1(args) shouldBe 1700
  }
}
