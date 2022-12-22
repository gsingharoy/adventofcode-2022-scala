package day13

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListMatcherSpec extends AnyFlatSpec with Matchers {
  "ListMatcher.removeBrackets" should "remove brackets when it is a single element" in {
    ListMatcher.removeBrackets("[1,1,3,1,1]") shouldEqual "1,1,3,1,1"

    ListMatcher.removeBrackets("[1,1],3,[1,1]") shouldEqual "[1,1],3,[1,1]"

    ListMatcher.removeBrackets("1,3") shouldEqual "1,3"

    ListMatcher.removeBrackets("1,[3,7]") shouldEqual "1,[3,7]"
  }

  "ListMatcher.compareStrLists" should "return true for the cases where it is valid" in {
    ListMatcher.compareStrLists(
      "[1,1,3,1,1]",
      "[1,1,5,1,1]"
    ) shouldEqual true

    ListMatcher.compareStrLists(
      "[[1],[2,3,4]]",
      "[[1],4]"
    ) shouldEqual true

    ListMatcher.compareStrLists(
      "[9]",
      "[[8,7,6]]"
    ) shouldEqual false

    ListMatcher.compareStrLists(
      "[[4,4],4,4]",
      "[[4,4],4,4,4]"
    ) shouldEqual true

    ListMatcher.compareStrLists(
      "[]",
      "[3]"
    ) shouldEqual true

    ListMatcher.compareStrLists(
      "[[[]]]",
      "[[]]"
    ) shouldEqual false

    ListMatcher.compareStrLists(
      "[1,[2,[3,[4,[5,6,7]]]],8,9]",
      "[1,[2,[3,[4,[5,6,0]]]],8,9]"
    ) shouldEqual false
  }

  "ListMatcher.breakStr" should "break into valid strings" in {
    ListMatcher.breakStr(
      "[1,1,3,1,1]"
    ) shouldEqual List("[1,1,3,1,1]")

    ListMatcher.breakStr(
      "[[1],[2,3,4]]"
    ) shouldEqual List("[[1],[2,3,4]]")

    ListMatcher.breakStr(
      "[1],[2,3,4]"
    ) shouldEqual List("[1]","[2,3,4]")

    ListMatcher.breakStr(
      "[4,4],4,[4],4"
    ) shouldEqual List("[4,4]", "4", "[4]", "4")

    ListMatcher.breakStr(
      ""
    ) shouldEqual List.empty

    ListMatcher.breakStr(
      "1"
    ) shouldEqual List("1")
  }
}
