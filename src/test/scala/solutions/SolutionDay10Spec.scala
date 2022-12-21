package solutions

import common.FileUtils
import day10.Solution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay10Spec extends AnyFlatSpec with Matchers {

 "Day 10: Sample" should "return the correct sum of the signals" in  {
   val args = FileUtils.readFile("day10/sample")

   Solution.part1(args) shouldEqual 13140
 }

  "Day 10: Part one" should "return the correct sum of the signals" in {
    val args = FileUtils.readFile("day10/input")

    Solution.part1(args) shouldEqual 14040
  }
}
