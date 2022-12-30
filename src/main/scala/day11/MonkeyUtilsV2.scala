package day11

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class MonkeyUtilsV2(monkeys: List[Monkey],
                         rounds: Int,
                         smartCorrector: Boolean = false) {

  private lazy val itemsMap: mutable.HashMap[Int, (ArrayBuffer[Int], ArrayBuffer[BigInt])] = mutable.HashMap.empty


  def calculateMonkeyBusiness(): Option[BigInt] = {
    makeAllMoves()
    itemsMap.toList.map(_._2._1.toList.length).sortWith(_ > _) match {
      case l if l.length < 2 => None
      case head1 :: head2 :: _ => Some( head1 * head2)
    }
  }

  private def initializeMap(): Unit = {
    monkeys.foreach(m =>
      itemsMap.addOne((m.id, (ArrayBuffer[Int](), ArrayBuffer[BigInt]() ++ m.items)))
    )
    println(itemsMap)
  }

  private lazy val denominations: List[Int] = monkeys.map(_.test.divisibleDenominator)


  private def makeAllMoves(): Unit = {
    initializeMap()

    Range.inclusive(1, rounds).foreach(currRound => {
      monkeys.sortWith(_.id < _.id).foreach( monkey =>{
        makeMoves(monkey)
      })
      //println(itemsMap)
      println(s"Finished playing for round ${currRound}")
    })
  }

  private def makeMoves(monkey: Monkey): Unit = {
    itemsMap.get(monkey.id).foreach(l => {
      l._2.foreach(i => {
        val (newIndex, newWorryItem) = moveItem(monkey, i)
        //println(s"Monekey ${monkey.id} has thrown ${newWorryItem} to ${newIndex}")
        addWorryItem(newIndex, newWorryItem)
        l._1 += newIndex
      }
      )
      // empty the current set of items
      l._2.clear()
      }
    )
  }
  private def addWorryItem(id: Int, worryItem: BigInt): Unit =
    itemsMap.get(id).map( _._2 += worryItem )

  /**
   * Returns the id of the monkey to make the move to
   *
   * @param monkey
   * @param item
   * @return
   */
  private def moveItem(monkey: Monkey, item: BigInt): (Int, BigInt) = {
    // First make the calculation to the Operation on the item and then divide it by worryDivider and round to the nearest integer
    val newWorry = {
      if (smartCorrector)
        {
          WorryUtils.applyCorrections(monkey.operation.calc(item), denominations)
        }
      else {
        // This is the custom corrector logic
        monkey.operation.calc(item) / 3
      }
    }
    if (newWorry % monkey.test.divisibleDenominator == 0) {
      (monkey.test.trueCase, newWorry)
    } else {
      (monkey.test.falseCase, newWorry)
    }
  }

}
