package day11

import common.{FileUtils, ListUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonkeySpec extends AnyFlatSpec with Matchers {
  "MonkeyTest.fromStrings" should "able to form a MonkeyTest for correct values" in {
    val args1 = List(
      "  Test: divisible by 17",
      "    If true: throw to monkey 0",
      "   If false: throw to monkey 1"
    )

    MonkeyTest.fromStrings(args1) shouldEqual Some(MonkeyTest(17, 0, 1))
  }

  "MonkeyTest.fromStrings" should "return None for ill formed input" in {
    val args1 = List(
      "  Test: divisible by 17",
      "    If true: throw to monkey 0",
      "   If false: throw to monkey 1",
      "   If false: throw to monkey 2"
    )

    MonkeyTest.fromStrings(args1) shouldEqual None

    val args2 = List(
      "  Tes: divisible by 17",
      "    If true: throw to monkey 0",
      "   If false: throw to monkey 1"
    )

    MonkeyTest.fromStrings(args2) shouldEqual None

    val args3 = List(
      "  Test: divisible by 17",
      "    If lala: throw to monkey 0",
      "   If false: throw to monkey 1"
    )

    MonkeyTest.fromStrings(args3) shouldEqual None
  }

  "OpObject.Constant" should "be able to always return constant" in {
    OpObject.Constant(78).o(90) shouldEqual 78
  }

  "OpObject.Old" should "be able to always return the value" in {
    OpObject.Old.o(89) shouldEqual 89
  }

  "MonkeyOperation.fromString" should "be able to construct an operation successfully" in {
    val mop1 = MonkeyOperation.fromString("  Operation: new = old + 6").get

    mop1.calc(10) shouldEqual 16
    mop1.calc(20) shouldEqual 26
    mop1.calc(100) shouldEqual(106)

    val mop2 = MonkeyOperation.fromString("  Operation: new = old * old").get

    mop2.calc(10) shouldEqual 100
    mop2.calc(20) shouldEqual 400
    mop2.calc(9) shouldEqual (81)
  }

  "MonkeyOperation.fromString" should "Return None for ill formed string" in {
    MonkeyOperation.fromString("  Operation : new = old + 6") shouldEqual None
    MonkeyOperation.fromString("  Operation: new = old + lala") shouldEqual None
  }

  "Monkey.fromString" should "be able to form a monkey" in {
    val args = List(
      "Monkey 2:",
      "  Starting items: 79, 98",
      "  Operation: new = old * 19",
      "  Test: divisible by 23",
      "    If true: throw to monkey 2",
      "   If false: throw to monkey 3"
    )

    val m = Monkey.fromStrings(args).get

    m.id shouldEqual 2
    m.items shouldEqual List(79, 98)
    m.operation.sub shouldEqual OpObject.Old
    m.operation.op shouldEqual MultiplyOp()
    m.operation.obj.o(800) shouldEqual 19
    m.test shouldEqual MonkeyTest(23, 2, 3)
  }

  "Monkey.fromString" should "be able to form multiple monkeys from the input file format" in {
    val args = ListUtils.zipListByPivotValue(FileUtils.readFile("day11/sample"),"")

    val monkeys = args.map(Monkey.fromStrings(_))

    monkeys.length shouldBe 4
  }

  "Monkey#moves" should "be able to return the expected moves" in {
    val args1 = List(
      "Monkey 0:",
      "  Starting items: 79, 98",
      "  Operation: new = old * 19",
      "  Test: divisible by 23",
      "    If true: throw to monkey 2",
      "   If false: throw to monkey 3"
    )

    val m1 = Monkey.fromStrings(args1).get

    m1.moves shouldBe List(
      ThrowOperation(3, 500),
      ThrowOperation(3, 620)
    )

    val args2 = List(
      "Monkey 1:",
      "  Starting items: 54, 65, 75, 74",
      "  Operation: new = old + 6",
      "  Test: divisible by 19",
      "    If true: throw to monkey 2",
      "   If false: throw to monkey 0"
    )

    val m2 = Monkey.fromStrings(args2).get

    m2.moves shouldBe List(
      ThrowOperation(0, 20),
      ThrowOperation(0, 23),
      ThrowOperation(0, 27),
      ThrowOperation(0, 26)
    )
  }
}
