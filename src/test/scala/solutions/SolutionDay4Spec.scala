package solutions

import common.FileUtils
import day4.{Assignment, Solution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay4Spec extends AnyFlatSpec with Matchers {

  "Solution Day 4: Part one" should "count successfully the pairs where there are complete overlaps" in {
    val args: List[String] = FileUtils.readFile("day4/input")

    Solution.part1(args) shouldEqual 305
  }

  "Solution Day 4: Part two" should "count successfully the pairs where there are partial overlaps" in {
    val args: List[String] = FileUtils.readFile("day4/input")

    Solution.part2(args) shouldEqual 811
  }

}
