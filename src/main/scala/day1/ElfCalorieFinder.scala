package day1

import scala.annotation.tailrec

object ElfCalorieFinder {


  /**
   *
   * @param elves Input elves
   * @return the elf which is holding the maximum number of calories
   */
  def findElfWithMaxCalories(elves: List[Elf]): Option[Elf] = elves match {
    case Nil => None
    case _ =>  Some(elves.max)
  }

  /**
   *
   * @param elves
   * @param totalTopElves
   * @return
   */
  def findTopElvesWithCalorificValues(elves: List[Elf], totalTopElves: Int): List[Elf] = {

    @tailrec
    def f(e: List[Elf], result: List[Elf]): List[Elf] = e match {
      case Nil => result
      case head :: tail =>
        if (result.length < totalTopElves )
          f(tail, result :+ head)
        else
          result
    }

    f(elves.sortWith(_.totalCalories > _.totalCalories), List.empty)
  }

}
