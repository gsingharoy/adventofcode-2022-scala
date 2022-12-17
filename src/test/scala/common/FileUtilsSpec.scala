package common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileUtilsSpec extends AnyFlatSpec with Matchers {
  "FileUtils.readFile" should "return the list of strings in the file " in {
    val words = FileUtils.readFile("common/input1.txt")

    words shouldEqual List("67", "738", "38839", "hello", "world" )
  }

}
