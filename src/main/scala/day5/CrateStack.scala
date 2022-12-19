package day5

import scala.annotation.tailrec

/**
 * Represents a Crate stack where multiple moves would be made by the crane
 *
 * @param id Unique integer identifier to identify the CrateStack
 * @param crates Represents the crates. Here the crates are such that the first element is the one on the top.
 */
case class CrateStack(id: Int, crates: List[Char]) extends Ordered[CrateStack] {

  /**
   * Function to remove some items from the CrateStack and also return the removed items
   *
   * @param totalItems
   * @return (List[Char], CrateStack) which is a tuple of the crates removed in the order they are supposed to be moved.
   *         The 2nd tuple, which is the CrateStack represents the left state of the Crate
   */
  def removeItems(totalItems: Int): (List[Char], CrateStack) = {

    @tailrec
    def f(items: List[Char], itemsLeft: Int, result: List[Char] = List.empty): (List[Char], CrateStack) =
      if (itemsLeft <= 0)
        (result, CrateStack(id, items))
      else
        items match {
          case Nil => (result, copy(crates = items))
          case head :: tail => f(tail, itemsLeft - 1, result :+ head)
        }

    f(crates, totalItems)
  }


  /**
   * Add items to the current crate stack. Please note that items are supposed to be added such that the first item goes
   * to the top of the pile
   *
   * @param items
   * @return
   */
  def addItems(items: List[Char]): CrateStack =
    copy(crates = items.reverse ++ this.crates)


  lazy val topItem: Option[Char] = crates.headOption

  override def compare(that: CrateStack): Int = this.id - that.id
}

object CrateStack {
  def constructCrateStacks(inputStrings: List[String]): List[CrateStack] = {
    if (inputStrings.isEmpty) List.empty
    else
      {
        val crateNumbers = CrateStackUtils.formStackNumbers(inputStrings.last)
        val stackValues: List[List[Char]] = inputStrings
          .dropRight(1)
          .map(CrateStackUtils.formStackCrates(_, crateNumbers.length))
          .transpose
          .map(_.flatten)
        val zippedValues: List[(Int, List[Char])] = crateNumbers.zip(stackValues)
        zippedValues.map(p => CrateStack(id = p._1, crates = p._2))
      }
  }
}
