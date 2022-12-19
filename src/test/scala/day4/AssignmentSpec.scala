package day4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssignmentSpec extends AnyFlatSpec with Matchers {
  "Assignment.constructFromString" should "return None for improper strings" in {

    Assignment.constructFromString("5-6,9-10,80-10") shouldEqual None

    Assignment.constructFromString("5-6,90-8") shouldEqual None

  }

  "Assignment.constructFromString" should "return the correct Assignments for proper assignment pairs" in {

    Assignment.constructFromString("2-4,6-8") shouldEqual Some(Assignment(List(2,3,4), List(6,7,8)))

  }

  "Assignment#hasCompleteOverlap" should "return true for the cases where there are overlaps" in {

    val rawStrings: List[String] = List[String]("2-8,3-7", "6-6,4-6")
    for (str <- rawStrings) {
      val ass = Assignment.constructFromString(str).get
      ass.hasCompleteOverlap shouldEqual true
    }

  }

  "Assignment#hasCompleteOverlap" should "return false for the cases where there are NO overlaps" in {

    val rawStrings: List[String] = List[String]("2-4,6-8", "2-3,4-5", "5-7,7-9","2-6,4-8")
    for (str <- rawStrings) {
      val ass = Assignment.constructFromString(str).get
      ass.hasCompleteOverlap shouldEqual false
    }

  }

  "Assignment#hasPartialOverlaps" should "return true for the cases where there are overlaps in some of the work" in {

    val rawStrings: List[String] = List[String]("5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8")
    for (str <- rawStrings) {
      val ass = Assignment.constructFromString(str).get
      ass.hasPartialOverlap shouldEqual true
    }

  }

  "Assignment#hasPartialOverlaps" should "return false for the cases where there are NO overlaps" in {

    val rawStrings: List[String] = List[String]("2-4,6-8", "2-3,4-5")
    for (str <- rawStrings) {
      val ass = Assignment.constructFromString(str).get
      ass.hasPartialOverlap shouldEqual false
    }

  }
}
