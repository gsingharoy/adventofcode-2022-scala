package day5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrateStackSpec extends AnyFlatSpec with Matchers {
  "CrateStack#removeItems" should "Remove the items in the order they are supposed to be removed" in {
    val cs1: CrateStack = CrateStack(1, List('D', 'C', 'M'))

    val result1 = cs1.removeItems(1)

    result1._1 shouldEqual List('D')
    result1._2 shouldEqual CrateStack(1, List('C', 'M'))

    val cs2: CrateStack = CrateStack(1, List('D', 'N', 'Z'))

    val result2 = cs2.removeItems(2)

    result2._1 shouldEqual List('D', 'N')
    result2._2 shouldEqual CrateStack(1, List('Z'))
  }

  "CrateStack#addItems" should "be able to add items successfully in the order they are supposed to be added" in {
    val cs1: CrateStack = CrateStack(1, List('N', 'Z'))

    val result1 = cs1.addItems(List('D'))

    result1 shouldEqual CrateStack(1, List('D', 'N', 'Z'))

    val cs2: CrateStack = CrateStack(1, List('P'))

    val result2 = cs2.addItems(List('D', 'N', 'Z'))

    result2 shouldEqual CrateStack(1, List('Z', 'N', 'D', 'P'))
  }

  "CrateStack#constructStacks" should "be able to construct a list of stacks based on string input" in {
    val strStacks: List[String] = List(
      "    [D]",
      "[N] [C]",
      "[Z] [M] [P]",
      " 1   2   3 "
    )

    CrateStack.constructCrateStacks(strStacks) shouldEqual List(
      CrateStack(1, List('N', 'Z')),
      CrateStack(2, List('D', 'C', 'M')),
      CrateStack(3, List('P'))
    )
  }
}
