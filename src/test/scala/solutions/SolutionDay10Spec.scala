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

  "Day 10: Part two (sample)" should "print the pixels on the screen" in {
    val args = FileUtils.readFile("day10/sample")
    // This test does not match anything but prints the pixels on the screen
    Solution.part2(args)
  }

  "Day 10: Part two" should "print the pixels on the screen" in {
    val args = FileUtils.readFile("day10/input")
    // this test does not match anything but prints the pixels on the screen. You should see "ZGCJZJFL"
    Solution.part2(args)
  }
}
