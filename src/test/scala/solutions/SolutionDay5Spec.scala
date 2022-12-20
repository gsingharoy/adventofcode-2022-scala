package solutions

import common.FileUtils
import day5.{CrateStack, CrateStackUtils, MoveAction, Solution}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class SolutionDay5Spec extends AnyFlatSpec with Matchers {

  "Day 5 : Part one" should "be able to find the top most crates in each stack" in {
    val args = FileUtils.readFile("day5/input")

    Solution.part1(args) shouldEqual "SPFMVDTZT"

  }

  "Day 5 : Part two" should "be able to find the top most crates in each stack when using an advanced 9001 crane" in {
    val args = FileUtils.readFile("day5/input")

    Solution.part2(args)shouldEqual "ZFSJBPRFP"

  }

}
