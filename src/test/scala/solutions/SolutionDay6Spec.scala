package solutions

import common.FileUtils
import day6.{Device, Solution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay6Spec extends AnyFlatSpec with Matchers {
  "Day 6 : Part one" should "be able to find the start of the marker" in {
    val args = FileUtils.readFile("day6/input")

    Solution.part1(args) shouldBe Some(1953)
  }

  "Day 6 : Part two" should "be able to find the start of the marker" in {
    val args = FileUtils.readFile("day6/input")

    Solution.part2(args) shouldBe Some(2301)
  }
}
