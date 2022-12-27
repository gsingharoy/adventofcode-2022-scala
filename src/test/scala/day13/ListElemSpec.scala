package day13

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListElemSpec extends AnyFlatSpec with Matchers {
  "ListElem.breakStr" should "break into valid strings" in {
    ListElem.breakStr(
      "[1,1,3,1,1]"
    ) shouldEqual List("[1,1,3,1,1]")

    ListElem.breakStr(
      "[[1],[2,3,4]]"
    ) shouldEqual List("[[1],[2,3,4]]")

    ListElem.breakStr(
      "[1],[2,3,4]"
    ) shouldEqual List("[1]", "[2,3,4]")

    ListElem.breakStr(
      "[4,4],4,[4],4"
    ) shouldEqual List("[4,4]", "4", "[4]", "4")

    ListElem.breakStr(
      ""
    ) shouldEqual List.empty

    ListElem.breakStr(
      "1"
    ) shouldEqual List("1")
  }

  "ListElem.removeBrackets" should "remove brackets when it is a single element" in {
    ListElem.removeBrackets("[1,1,3,1,1]") shouldEqual "1,1,3,1,1"

    ListElem.removeBrackets("[1,1],3,[1,1]") shouldEqual "[1,1],3,[1,1]"

    ListElem.removeBrackets("1,3") shouldEqual "1,3"

    ListElem.removeBrackets("1,[3,7]") shouldEqual "1,[3,7]"
  }


  "ListElem#children" should "return the correct children" in {
    ListElem("[[1],[2,3,4]]", ListElemPosition.Start, None)
      .children shouldEqual List(
      ListElem("[1]", ListElemPosition(1, 0), Some(ListElemPosition.Start)),
      ListElem("[2,3,4]", ListElemPosition(1, 1), Some(ListElemPosition.Start))
    )

    ListElem("1,[3,7]", ListElemPosition.Start, None)
      .children shouldEqual List.empty

    ListElem("[1,[3,7]]", ListElemPosition.Start, None)
      .children shouldEqual List(
      ListElem("1", ListElemPosition(1, 0), Some(ListElemPosition.Start)),
      ListElem("[3,7]", ListElemPosition(1, 1), Some(ListElemPosition.Start))
    )

  }
}
