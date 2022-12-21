package day11

import day9.Move

import scala.collection.SeqView.Sorted

/**
 * Represents a monkey
 *
 * @param id unique identifier of the monkey. The value is 0 to 9
 * @param items
 * @param operation
 * @param test
 */
case class Monkey(id: Int,
                  items: List[BigInt],
                  operation: MonkeyOperation,
                  test: MonkeyTest,
                  totalItemsInspected: Long = 0, // total times this particular monkey has inspected an item
                  smartCorrector: Boolean = false)  {

  /**
   * Return a list of ThrowOperations in the order the items are evaluated.
   */
  lazy val moves: List[ThrowOperation] = items.map(move)

  /**
   * Make a move on an item.
   * @param item
   * @return
   */
  private def move(item: BigInt): ThrowOperation = {
    // First make the calculation to the Operation on the item and then divide it by worryDivider and round to the nearest integer
    val newWorry =  {
      if (smartCorrector)
        operation.calc(item)
      else {
        // This is the custom corrector logic
        operation.calc(item) / 3
      }
    }
        if (newWorry % test.divisibleDenominator == 0) {
          ThrowOperation(monkeyId = test.trueCase, worryItem = newWorry)
        } else {
          ThrowOperation(monkeyId = test.falseCase, worryItem = newWorry)
        }
  }

  def applyThrowAction(action: ThrowOperation): Monkey = if (action.monkeyId == id) {
    this.copy(items = this.items :+ action.worryItem)
  } else {
    this
  }

  lazy val inspectItems: (Monkey, List[ThrowOperation]) =
    (emptyItems.copy(totalItemsInspected = this.totalItemsInspected + moves.length), moves)

  private lazy val emptyItems: Monkey = this.copy(items = List.empty)

}

object Monkey extends Ordering[Monkey] {

  /**
   * Try to construct a Monkey by parsing the input of strings.
   *
   * @param strings
   * @return
   */
  def fromStrings(strings: List[String], smartCorrector: Boolean = false): Option[Monkey] = {

    def monkeyId(str: String): Option[Int] = str match {
      case s"Monkey ${sId}:" => sId.toIntOption
      case _ => None
    }

    def monkeyItems(str: String): List[BigInt] = str match {
      case s"Starting items: ${sItems}" => sItems.split(",").toList.map(_.trim).flatMap(_.toIntOption).map(BigInt(_))
      case _ => List.empty
    }

    strings.map(_.trim) match {
      case l if l.length != 6 => None // ill formed strings
      case head :: itemsStr :: opStr :: testStrings =>
        (monkeyId(head), monkeyItems(itemsStr), MonkeyOperation.fromString(opStr), MonkeyTest.fromStrings(testStrings)) match {
          case (Some(id), items, Some(op), Some(mt)) if items.nonEmpty => Some(
            Monkey(id = id, items = items, operation = op, test = mt, smartCorrector = smartCorrector, totalItemsInspected = 0)
          )
          case _ => None // result of an unsuccessful formation of a monkey
        }
    }
  }

  override def compare(x: Monkey, y: Monkey): Int = x.id - y.id
}

/**
 * Represents a case class for an operation such that
 * sub op obj.
 * eg, if sub is old, obj is 90 and op is * then "old * 90"
 *
 * @param sub
 * @param op
 * @param obj
 */
case class MonkeyOperation(sub: OpObject, op: Operator, obj: OpObject) {

  /**
   * Make a calculation with the passed old value
   *
   * @param old
   * @return
   */
  def calc(old: BigInt): BigInt = op match {
    case _: MultiplyOp => sub.o(old) * obj.o(old)
    case _: AddOp => sub.o(old) + obj.o(old)
    case _ => -1 // This is an error case. Ideally it should not exist as op is a typed value and no funny values would be injected
  }
}

object MonkeyOperation {

  def fromString(str: String): Option[MonkeyOperation] = str.trim match {
    case s"Operation: new = ${sSub} ${sOp} ${sObj}" =>
      (OpObject.fromString(sSub), Operator.fromString(sOp), OpObject.fromString(sObj)) match {
        case (Some(sub), Some(op), Some(obj)) => Some(MonkeyOperation(sub = sub, op = op, obj = obj))
        case _ => None
      }
    case _ => None
  }
}

trait Operator

object Operator {

  def fromString(str: String): Option[Operator] = str match {
    case "+" => Some(AddOp())
    case "*" => Some(MultiplyOp())
    case _ => None
  }
}

case class OpObject(o: BigInt => BigInt)

object OpObject {

  def Constant(i: Int): OpObject = OpObject({ _ => i})
  def Old: OpObject = OpObject({i => i})

  def fromString(str: String): Option[OpObject] = str match {
    case "old" => Some(Old)
    case o => o.toIntOption.map(Constant)
  }
}

case class MultiplyOp() extends Operator

case class AddOp() extends Operator


case class MonkeyTest(divisibleDenominator: Int, trueCase: Int, falseCase: Int)

object MonkeyTest {

  def fromStrings(strings: List[String]): Option[MonkeyTest] = strings.map(_.trim) match {
    case l if (l.length != 3) => None // wrong number of argumments
    case s"Test: divisible by ${sDenom}" :: s"If true: throw to monkey ${sTrueCase}" :: s"If false: throw to monkey ${sFalseCase}" :: _ =>
      (sDenom.toIntOption, sTrueCase.toIntOption, sFalseCase.toIntOption) match {
        case (Some(denom), Some(trueCase), Some(falseCase)) => Some(
          MonkeyTest(divisibleDenominator = denom, trueCase = trueCase, falseCase = falseCase)
        )
        case _ => None
      }
    case _ => None
  }
}

case class ThrowOperation(monkeyId: Int, worryItem: BigInt)
