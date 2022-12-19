package day4

object AssignmentUtils {

  // Expands shelves from two integer points
  def constructShelves(x: Int, y: Int): List[Int] =
      Range.inclusive(x, y).toList


  /**
   * Constructs the shelves with integer values based on the strings.
   * @param str needs to be of a format 4-15.
   * @return Returns None for invalid inputs
   */
  def constructShelves(str: String): Option[List[Int]] = str.split("-").toList match {
    case s if s.length != 2 => None // ill formed string
    case head1 :: head2 :: _ =>  (head1.toIntOption, head2.toIntOption) match {
      case (Some(x), Some(y)) if (x <= y) => Some(constructShelves(x, y))
      case _ => None
    }
  }
}
