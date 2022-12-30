package day11

import scala.annotation.tailrec

object WorryUtils {

  def applyCorrections(item: BigInt, divisibleDenominators: List[Int]): BigInt = {

    @tailrec
    def corr(item: BigInt, divisibleDenominator: Int): BigInt = item match {
      case i if (i % (divisibleDenominator * divisibleDenominator) == 0) =>
        corr(i / divisibleDenominator, divisibleDenominator) // apply correction
      case i => i
    }

    @tailrec
    def f(denominations: List[Int], result: BigInt): BigInt = denominations match {
      case Nil          => result
      case head :: tail => f(tail, corr(result, head))
    }

    val result = f(divisibleDenominators, item)
    if (result != item)
      println(
        s"A correction was applied to the item and the item is now ${item / result} times smaller"
      )
    result
  }
}
