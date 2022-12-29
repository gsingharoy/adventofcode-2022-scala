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
    ListElem("[[1],[2,3,4]]", ListElemPosition.Start)
      .children shouldEqual List(
      ListElem("[1]", ListElemPosition("0", 0)),
      ListElem("[2,3,4]", ListElemPosition("0", 1))
    )

    ListElem("1,[3,7]", ListElemPosition.Start)
      .children shouldEqual List.empty

    ListElem("[1,[3,7]]", ListElemPosition.Start)
      .children shouldEqual List(
      ListElem("1", ListElemPosition("0", 0)),
      ListElem("[3,7]", ListElemPosition("0", 1))
    )

    ListElem("[1,[3,7]]", ListElemPosition("1", 10))
      .children shouldEqual List(
      ListElem("1", ListElemPosition("1/10", 0)),
      ListElem("[3,7]", ListElemPosition("1/10", 1))
    )

    ListElem("[1]", ListElemPosition("0/0", 0))
      .children shouldEqual List(
      ListElem("1", ListElemPosition("0/0/0", 0))
    )

    ListElem("[]", ListElemPosition("0/1", 3)).children shouldEqual List.empty

    ListElem("[[[[],8],1]]", ListElemPosition.Start).children shouldEqual List(
      ListElem("[[[],8],1]", ListElemPosition("0", 0))
    )
  }

  "ListElemPosition#parent" should "return the correct parent position" in {

    ListElemPosition("0/1/10/20", 3).parentPos shouldEqual Some(
      ListElemPosition("0/1/10", 20)
    )
  }

    "ListElemPosition#childDepth" should "return the correct children's depth" in {
      ListElemPosition("0/1/10/20", 3).childDepth shouldEqual "0/1/10/20/3"
      ListElemPosition.Start.childDepth shouldEqual "0"
    }

}
