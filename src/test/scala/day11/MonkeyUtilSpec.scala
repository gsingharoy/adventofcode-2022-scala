package day11

import common.{ ListUtils, FileUtils }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonkeyUtilSpec extends AnyFlatSpec with Matchers {
  "MonkeyUtil.makeMoves" should "be able to make the moves of the monkey" in {
    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")

    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_))

    val (resultMoves, resultMonkeys) = MonkeyUtils.makeMoves(monkeys.head, monkeys)

    resultMoves shouldEqual List(
      ThrowOperation(3, 500),
      ThrowOperation(3, 620)
    )

    resultMonkeys.find(_.id == 0).get.items shouldEqual List.empty
    resultMonkeys.find(_.id == 1).get.items shouldEqual monkeys.find(_.id == 1).get.items
    resultMonkeys.find(_.id == 2).get.items shouldEqual monkeys.find(_.id == 2).get.items
    resultMonkeys.find(_.id == 3).get.items shouldEqual monkeys.find(_.id == 3).get.items ++ List(500, 620)
  }

  "MonkeyUtil.completeRound" should "be able to make all the moves in a round for all the monkeys" in {
    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")

    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_))

    val resultMonkeys = MonkeyUtils.completeRound(monkeys)

    resultMonkeys.find(_.id == 0).get.items shouldEqual List(20, 23, 27, 26)
    resultMonkeys.find(_.id == 1).get.items shouldEqual List(2080, 25, 167, 207, 401, 1046)
    resultMonkeys.find(_.id == 2).get.items shouldEqual List.empty
    resultMonkeys.find(_.id == 3).get.items shouldEqual List.empty

    resultMonkeys.find(_.id == 0).get.totalItemsInspected shouldEqual 2
    resultMonkeys.find(_.id == 1).get.totalItemsInspected shouldEqual 4
    resultMonkeys.find(_.id == 2).get.totalItemsInspected shouldEqual 3
    resultMonkeys.find(_.id == 3).get.totalItemsInspected shouldEqual 5

    val resultMonkeys2 = MonkeyUtils.completeRound(resultMonkeys)

    resultMonkeys2.find(_.id == 0).get.items shouldEqual List(695, 10, 71, 135, 350)
    resultMonkeys2.find(_.id == 1).get.items shouldEqual List(43, 49, 58, 55, 362)
    resultMonkeys2.find(_.id == 2).get.items shouldEqual List.empty
    resultMonkeys2.find(_.id == 3).get.items shouldEqual List.empty

    val resultMonkeys3 = MonkeyUtils.completeRound(resultMonkeys2)

    resultMonkeys3.find(_.id == 0).get.items shouldEqual List(16, 18, 21, 20, 122)
    resultMonkeys3.find(_.id == 1).get.items shouldEqual List(1468, 22, 150, 286, 739)
    resultMonkeys3.find(_.id == 2).get.items shouldEqual List.empty
    resultMonkeys3.find(_.id == 3).get.items shouldEqual List.empty
  }

  "MonkeyUtil.completeInspection" should "be able to make all the moves in all the rounds" in {
    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")

    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_))

    val resultMonkeys = MonkeyUtils.completeInspections(monkeys, 20)

    resultMonkeys.find(_.id == 0).get.totalItemsInspected shouldEqual 101
    resultMonkeys.find(_.id == 1).get.totalItemsInspected shouldEqual 95
    resultMonkeys.find(_.id == 2).get.totalItemsInspected shouldEqual 7
    resultMonkeys.find(_.id == 3).get.totalItemsInspected shouldEqual 105
  }

  "MonkeyUtil.calculateMonkeyBusiness" should "be able to calculate the monkey business successfully" in {
    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")

    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_))

    MonkeyUtils.calculateMoneeyBusiness(monkeys, 20) shouldEqual Some(10605)
  }


//  "MonkeyUtil.completeInspection" should "be able to make all the moves in all the rounds after removing the worry divider" in {
//    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")
//
//    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_, smartCorrector = true))
//
//
//    val resultMonkeys = MonkeyUtils.completeInspections(monkeys, 1000, smartCorrector = true)
//
//    resultMonkeys.find(_.id == 0).get.totalItemsInspected shouldEqual 5204
//    resultMonkeys.find(_.id == 1).get.totalItemsInspected shouldEqual 4792
//    resultMonkeys.find(_.id == 2).get.totalItemsInspected shouldEqual 199
//    resultMonkeys.find(_.id == 3).get.totalItemsInspected shouldEqual 5192
//  }

//  "MonkeyUtil.calculateMonkeyBusiness" should "be able to calculate the monkey business successfully removing the worry divider" in {
//    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")
//
//    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_, 1))
//
//    MonkeyUtils.calculateMoneeyBusiness(monkeys, 10000) shouldEqual Some(2713310158L)
//  }
}
