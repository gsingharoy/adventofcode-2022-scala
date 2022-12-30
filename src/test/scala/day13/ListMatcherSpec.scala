package day13

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListMatcherSpec extends AnyFlatSpec with Matchers {

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

    ListMatcher.compareStrLists(
      "[[[]],[5,[[6,3,1],[5,1,3],1,[8,10,5,9,10]],7,6,[4,5,6,[]]]]",
      "[[[]],[3,[[],[2,4,9,7,6],[1,9,10,1]],4,[2,[4],[],[0,1,9]],1],[],[1,7,2],[[7,[2,5,1,4],[],7,6]]]"
    ) shouldEqual false
  }

  "ListMatcher.compareStrLists" should "return true for the cases taken from the actual inputs where it is valid" in {
    ListMatcher.compareStrLists(
      "[[[]],[5,[[6,3,1],[5,1,3],1,[8,10,5,9,10]],7,6,[4,5,6,[]]]]",
      "[[[]],[3,[[],[2,4,9,7,6],[1,9,10,1]],4,[2,[4],[],[0,1,9]],1],[],[1,7,2],[[7,[2,5,1,4],[],7,6]]]"
    ) shouldEqual false

    ListMatcher.compareStrLists(
      "[[6,[[3,6,7,5],9,[8,2,4],4]],[7,[[1,3,2,5,6],[1,7],8],[],10,2],[[[8,6,3,10]],[[9,5,6,2,4],0],[5,[2,9,5],[6,7,10,3],7]],[5,0,[2,[2,8],4],1]]",
      "[[[[6,5],[8,3,10,3],0],[[4,4,7],6],0,[5,6,[7]]],[[[1,8,9,2],7,[9]],[2,[2]],0,[6,6],0],[[[3,3,4,2],[10,8],[2],[9,0]]]]"
    ) shouldEqual true

    ListMatcher.compareStrLists(
      "[[[[],8],1]]",
      "[[[[]]],[[[0,6,8]],[],1,[[4,5,6,7]]],[[5,[10,10,8],[5,4,2,10,5],[4,10,7,0]],7,5,[6,[4,6,8,7,8],[4,8,6]]],[0,5],[9]]"
    ) shouldEqual false

    ListMatcher.compareStrLists(
      "[[[],[[7,8,2]],0,9,7]]",
      "[[[],[10,[6,3],[1,8,1]]]]"
    ) shouldEqual true

    ListMatcher.compareStrLists(
      "[[[],[[7,8,2]],0,9,7]]",
      "[[[],[10,[6,3],[1,8,1]]]]"
    ) shouldEqual true

    ListMatcher.compareStrLists(
      "[[2,[],[[1,9,1,2],8]]]",
      "[[2,[],8,[9,[10,5],[9],1]],[5]]"
    ) shouldEqual true

  }


}
