package solutions

import common.FileUtils
import day15.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay15Spec extends AnyFlatSpec with Matchers {
  "Day 15: Part one" should "be able to count all the beacon exclusion units" in {
    val args = FileUtils.readFile("day15/input")

    Solution.part1(args) shouldEqual 5240818
  }

  "Day 15: Part two" should "be able to triangulate the distress signal location" in {
    val args = FileUtils.readFile("day15/input")

    Solution.part2(args) shouldEqual 13213086906101L
  }
}
