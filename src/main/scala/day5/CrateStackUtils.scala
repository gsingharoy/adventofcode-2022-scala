package day5

import scala.annotation.tailrec

object CrateStackUtils {

  /** @param move
    * @param crateStacks
    * @return
    */
  def moveItems(
      move: MoveAction,
      crateStacks: List[CrateStack],
      with9001: Boolean = false
  ): Option[List[CrateStack]] = {
    (crateStacks.find(_.id == move.sourceStack), crateStacks.find(_.id == move.sinkStack)) match {
      case (Some(sourceCrate), Some(sinkCrate)) => {
        val (itemsToRemove, newSourceCrate) = sourceCrate.removeItems(move.moveUnits)
        val newSinkCrate                    = sinkCrate.addItems(itemsToRemove, with9001)
        val newStackList =
          crateStacks.filter(cs => cs.id != move.sinkStack && cs.id != move.sourceStack)
        Some(newStackList ++ List(newSinkCrate, newSourceCrate))
      }
      case _ => None // No action could be performed as crates were not found in this case
    }
  }

  def makeMoves(
      movesList: List[MoveAction],
      crateStacks: List[CrateStack],
      with9001: Boolean = false
  ): (Int, List[CrateStack]) = {

    @tailrec
    def f(m: List[MoveAction], cs: List[CrateStack], resultSuccess: Int): (Int, List[CrateStack]) =
      m match {
        case Nil => (resultSuccess, cs.sorted)
        case head :: tail =>
          moveItems(head, cs, with9001) match {
            case Some(c) => f(tail, c, resultSuccess + 1)
            case None    => f(tail, cs, resultSuccess)
          }
      }

    f(movesList, crateStacks, 0)
  }

  def formStackCrates(inputString: String, totalCrates: Int): List[Option[Char]] = {
    val tempList = inputString.toList.grouped(4).toList.map {
      case l if (l.length < 3 && l.length > 4)              => None
      case _ :: c :: _ if (c.toInt >= 65 && c.toInt <= 100) => Some(c)
      case _                                                => None
    }
    if (tempList.length >= totalCrates)
      tempList
    else
      tempList ++ Range.inclusive(1, totalCrates - tempList.length).toList.map(_ => None)
  }

  def formStackNumbers(inputString: String): List[Int] = {
    inputString.toList.flatMap(_.toInt match {
      case i if (i >= 49 && i <= 57) => Some(i - 48)
      case i                         => None
    })
  }

  def topCrates(crateStacks: List[CrateStack]): String =
    crateStacks.flatMap(_.crates.headOption).mkString

}
