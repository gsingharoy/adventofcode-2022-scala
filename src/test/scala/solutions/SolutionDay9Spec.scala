package solutions

import common.FileUtils
import day9.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay9Spec extends AnyFlatSpec with Matchers {
  "Day 9 : Part one" should "be able to find the total unique tail positions" in {
    val args = FileUtils.readFile("day9/input")

    Solution.part1(args) shouldBe 6044
  }

  "Day 9 : Part two" should "be able to find the total unique tail positions of the last item in the tail" in {
    val args = FileUtils.readFile("day9/input")

    Solution.part2(args) shouldBe 2384
  }
}
