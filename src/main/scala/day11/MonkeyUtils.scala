package day11

import scala.annotation.tailrec

object MonkeyUtils {


  /**
   * Utility function to calculate monkey business. This makes total moves and in the end takes the top two monkeys
   * which have inspected the most items and returns the result
   *
   * @param allMonkeys
   * @param rounds
   * @return
   */
  def calculateMoneeyBusiness(allMonkeys: List[Monkey], rounds: Int, smartCorrector: Boolean = false): Option[BigInt] =
    completeInspections(allMonkeys, rounds, smartCorrector).map(_.totalItemsInspected).sortWith(_ > _) match {
    case l if l.length < 2 => None
    case top1 :: top2 :: _ => Some(top1 * top2)
  }
  /**
   * Complete all the inspections
   *
   * @param allMonkeys
   * @param totalRounds total number of rounds to be played
   * @return
   */
  def completeInspections(allMonkeys: List[Monkey], totalRounds: Int, smartCorrector: Boolean = false): List[Monkey] = {

    @tailrec
    def fMonkey(monkeys: List[Monkey], currRound: Int): List[Monkey] = currRound match {
      case c if (c > totalRounds) => monkeys // reached the end
      case c => {
        val nMonkeys = completeRound(monkeys, smartCorrector)
        println(s"Completed round ${c}...")
        fMonkey(nMonkeys, c + 1)
      }
    }

    fMonkey(allMonkeys, 1)
  }

  /**
   * Completes one full round of moves
   *
   * @param allMonkeys
   * @return
   */
  def completeRound(allMonkeys: List[Monkey], smartCorrector:Boolean = false): List[Monkey] = {

    @tailrec
    def fMonkey(ids: List[Int], monkeys: List[Monkey]): List[Monkey] = ids match {
      case Nil => monkeys
      case head :: tail => fMonkey(
        ids = tail,
        monkeys = monkeys.find(_.id == head).map(m => makeMoves(m, monkeys, smartCorrector)._2).getOrElse(monkeys)
      )

    }

    fMonkey(allMonkeys.map(_.id).sorted, allMonkeys)

  }
  /**
   * Make moves for a particular monkey. The move continues till all the items of the monkey becomes empty
   *
   * @param monkey
   * @param allMonkeys
   * @return
   */
  def makeMoves(monkey: Monkey, allMonkeys: List[Monkey], smartCorrector: Boolean = false): (List[ThrowOperation], List[Monkey]) = {
    val (currMonkey, actions) = monkey.inspectItems match {
      case (m, a) if smartCorrector => {
        val divisibleDenominators: List[Int] = allMonkeys.map(_.test.divisibleDenominator)
        // apply a smart correction to remove any "squares" in the items so that it does not go out of hand
        (m, a.map(to => to.copy(worryItem = WorryUtils.applyCorrections(to.worryItem, divisibleDenominators))))
      }
      case (m, a) => (m, a)
    }

    // empty the items of the current monkey
    val updatedMonkeys = allMonkeys.map({
      case m if m.id == monkey.id => currMonkey
      case m => m
    })

    @tailrec
    def reconstruct(input: List[ThrowOperation],
                    monkeys: List[Monkey]): List[Monkey] = input match {
      case Nil => monkeys
      case head :: tail => reconstruct(tail, applyAction(head, monkeys))
    }

    (actions, reconstruct(actions, updatedMonkeys))
  }

  private def applyAction(action: ThrowOperation, allMonkeys: List[Monkey]): List[Monkey] =
    allMonkeys.map(_.applyThrowAction(action))




}
