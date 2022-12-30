package day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RucksackUtilsSpec extends AnyFlatSpec with Matchers {

  "Rucksackutils.itemScore" should "return the expected score for lower case elements" in {
    RucksackUtils.itemScore('a') shouldEqual 1
    RucksackUtils.itemScore('z') shouldEqual 26
    RucksackUtils.itemScore('p') shouldEqual 16
  }

  "Rucksackutils.itemScore" should "return the expected score for upper case elements" in {
    RucksackUtils.itemScore('A') shouldEqual 27
    RucksackUtils.itemScore('Z') shouldEqual 52
    RucksackUtils.itemScore('P') shouldEqual 42
  }

  "Rucksackutils.itemScore" should "return 0 for invalid characters" in {
    RucksackUtils.itemScore('$') shouldEqual 0
    RucksackUtils.itemScore('1') shouldEqual 0
  }

}
