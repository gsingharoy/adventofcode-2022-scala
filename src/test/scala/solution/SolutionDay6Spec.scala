package solution

import common.FileUtils
import day6.Device
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay6Spec extends AnyFlatSpec with Matchers {
  "Day 6 : Part one" should "be able to find the start of the marker" in {
    val code: String = FileUtils.readFile("day6/input").head

    Device(code).startOfMarker shouldBe Some(1953)
  }

  "Day 6 : Part two" should "be able to find the start of the marker" in {
    val code: String = FileUtils.readFile("day6/input").head

    Device(code, 14).startOfMarker shouldBe Some(2301)
  }
}
