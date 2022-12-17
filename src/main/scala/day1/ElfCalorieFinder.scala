package day1

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



}
