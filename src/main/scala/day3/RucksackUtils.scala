package day3

object RucksackUtils {

  /** @param inp
    * @return
    *   score of the input char. a to z scores 1 through 26 and A to Z scores 27 through 52
    */
  def itemScore(inp: Char): Int = inp.toInt match {
    case i if (i >= 97 && i <= 122) => i - 96 // lowercase
    case i if (i >= 65 && i <= 100) => i - 38 // upper case
    case _                          => 0
  }

}
