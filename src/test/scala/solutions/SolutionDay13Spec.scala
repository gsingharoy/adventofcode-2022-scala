package solutions

import common.FileUtils
import day13.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay13Spec extends AnyFlatSpec with Matchers{
  "Day 13: Part one sample" should "be able to find the right sum of indices" in {
    val args = FileUtils.readFile("day13/sample")

    Solution.part1(args) shouldBe 13
  }

  "Day 13: Part one" should "be able to find the right sum of indices" in {
    val args = FileUtils.readFile("day13/input")

    Solution.part1(args) shouldBe 5761
  }
}
