package day5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrateStackUtilsSpec extends AnyFlatSpec with Matchers {

  "CrateStackUtils.moveItems" should "successfully move the expected items to the expected crate when it is found" in {
    val cs1 = CrateStack(1, List('N', 'Z'))
    val cs2 = CrateStack(2, List('D', 'C', 'M'))
    val cs3 = CrateStack(3, List('P'))

    val crateList1 = List(cs1, cs2, cs3)
    val move1 = MoveAction(sourceStack = 2, sinkStack = 1, moveUnits = 1)

    val resultCrates1 = CrateStackUtils.moveItems(move1, crateList1).get.sorted

    resultCrates1 shouldEqual List(
      CrateStack(1,List('D', 'N', 'Z')),
      CrateStack(2,List('C', 'M')),
      CrateStack(3,List('P'))
    )

    val move2 = MoveAction(sourceStack = 1, sinkStack = 3, moveUnits = 3)

    val resultCrates2 = CrateStackUtils.moveItems(move2, resultCrates1).get.sorted

    resultCrates2 shouldEqual List(
      CrateStack(1, List.empty),
      CrateStack(2, List('C', 'M')),
      CrateStack(3, List('Z', 'N', 'D', 'P'))
    )

    val move3 = MoveAction(sourceStack = 2, sinkStack = 1, moveUnits = 2)

    val resultCrates3 = CrateStackUtils.moveItems(move3, resultCrates2).get.sorted

    resultCrates3 shouldEqual List(
      CrateStack(1, List('M', 'C')),
      CrateStack(2, List.empty),
      CrateStack(3, List('Z', 'N', 'D', 'P'))
    )
  }

  "CreateStackUtils.formStackCrates" should "successfully form the stacks" in {
    val str1 = "    [H]         [D]     [P]"

    val result1 = CrateStackUtils.formStackCrates(str1, 9)

    result1 shouldEqual List(
      None,
      Some('H'),
      None,
      None,
      Some('D'),
      None,
      Some('P'),
      None,
      None
    )

    val str2 = "[F] [G] [H] [Z] [N] [P] [M] [N] [D]"

    val result2 = CrateStackUtils.formStackCrates(str2, 9)

    result2 shouldEqual List(
      Some('F'),
      Some('G'),
      Some('H'),
      Some('Z'),
      Some('N'),
      Some('P'),
      Some('M'),
      Some('N'),
      Some('D')
    )
  }

  "CrateStackUtils.formStackNumbers" should "form the correct stack numbers" in {
    CrateStackUtils.formStackNumbers(" 1   2   3   4   5   6   7   8   9 ") shouldEqual Range.inclusive(1,9).toList
  }
}
