package solutions

import common.FileUtils
import day11.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay11Spec extends AnyFlatSpec with Matchers {
  "Day 11: Part one" should "be able to successfully capture the monkey business" in {
    val args = FileUtils.readFile("day11/input")

    Solution.part1(args) shouldBe 58322
  }
}
