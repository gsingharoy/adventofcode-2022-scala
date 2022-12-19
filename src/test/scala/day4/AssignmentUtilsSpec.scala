package day4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssignmentUtilsSpec extends AnyFlatSpec with Matchers {
  "AssignmentUtils.constructShelves" should "construct shelves with integer range" in {
    val shelves = AssignmentUtils.constructShelves(2, 7)

    shelves shouldEqual List[Int](2, 3, 4, 5, 6, 7)
  }

  "AssignmentUtils.constructShelves" should "return none for ill formed strings" in {
    val shelves = AssignmentUtils.constructShelves("2-7-10")

    shelves shouldEqual None
  }

  "AssignmentUtils.constructShelves" should "return None for string with invalid range" in {
    val shelves = AssignmentUtils.constructShelves("7-2")

    shelves shouldEqual None
  }

  "AssignmentUtils.constructShelves" should "return Some of a list for string with a valid range" in {
    val shelves = AssignmentUtils.constructShelves("3-8")

    shelves shouldEqual Some(List[Int](3, 4, 5, 6, 7, 8))
  }
}
