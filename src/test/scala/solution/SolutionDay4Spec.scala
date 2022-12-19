package solution

import common.FileUtils
import day4.Assignment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay4Spec extends AnyFlatSpec with Matchers {

  "Solution Day 4: Part one" should "count successfully the pairs where there are complete overlaps" in {
    val lines: List[String] = FileUtils.readFile("day4/input")

    val assignments: List[Assignment] = lines.flatMap(Assignment.constructFromString)

    assignments.count(_.hasCompleteOverlap) shouldEqual 305
  }

  "Solution Day 4: Part two" should "count successfully the pairs where there are partial overlaps" in {
    val lines: List[String] = FileUtils.readFile("day4/input")

    val assignments: List[Assignment] = lines.flatMap(Assignment.constructFromString)

    assignments.count(_.hasPartialOverlap) shouldEqual 811
  }

}
