package day11

import common.{FileUtils, ListUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonkeyUtilsV2Spec extends AnyFlatSpec with Matchers{
  "MonkeyUtilsV2Spec#calculateMonkeyBusiness()" should "be able to calculate the monkey business for part one" in {
    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")

    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_))

    val mu = MonkeyUtilsV2(
      monkeys = monkeys,
      rounds = 20,
      smartCorrector = false
    )

    mu.calculateMonkeyBusiness() shouldEqual Some(10605)
  }

//  "MonkeyUtilsV2Spec#calculateMonkeyBusiness()" should "be able to calculate the monkey business for part two" in {
//    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"), "")
//
//    val monkeys: List[Monkey] = args.flatMap(Monkey.fromStrings(_))
//
//    val mu = MonkeyUtilsV2(
//      monkeys = monkeys,
//      rounds = 10000,
//      smartCorrector = true
//    )
//
//    mu.calculateMonkeyBusiness() shouldEqual Some(2_713_310_158)
//  }
}
