package day1

import scala.annotation.tailrec

// Basic model to define an elf
case class Elf(private val calories: List[Int]) {

  // Total calories an elf will carry
  val totalCalories: Int = calories.sum
}

object Elf {
  implicit def ordering[E <: Elf]: Ordering[E] = new Ordering[E] {
    override def compare(x: E, y: E): Int = x.totalCalories - y.totalCalories
  }

  /** @param calories
    *   list of string calories.
    * @return
    *   Return an Elf where first the string values are converted to an Int values. In case it is an
    *   invalid string, then that calorific value is ignored
    */
  def fromStrings(calories: List[String]): Elf = Elf(calories.flatMap(_.toIntOption))

  /** @param strElves
    * @return
    */
  def constructElves(strElves: List[String]): List[Elf] = {

    def attemptToCompleteElf(curr: String, cache: List[String]): Either[Elf, List[String]] =
      curr match {
        case "" => Left(Elf.fromStrings(cache.reverse))
        case _  => Right(curr :: cache)
      }

    @tailrec
    def f(
        elves: List[String],
        cache: List[String] = List.empty,
        result: List[Elf] = List.empty
    ): List[Elf] = elves match {
      case Nil => Elf.fromStrings(cache) :: result
      case head :: Nil =>
        attemptToCompleteElf(head, cache) match {
          case Left(e)  => e :: result
          case Right(s) => Elf.fromStrings(s.reverse) :: result
        }
      case head :: tail =>
        attemptToCompleteElf(head, cache) match {
          case Left(e)  => f(tail, List.empty, e :: result)
          case Right(s) => f(tail, s, result)
        }
    }

    f(strElves).reverse
  }
}
