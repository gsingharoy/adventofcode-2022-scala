package common

import scala.io.Source

object FileUtils {

  /** A function to read a file and return a list of string values
    *
    * @param filename
    *   string location of the text filename. The file should be in src/main/resources or
    *   src/test/resources
    * @returns
    *   a list of strings where each string is a line of the file
    */
  def readFile(filename: String): List[String] =
    Source.fromResource(filename).getLines().toList

}
