package day7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OSFileSpec extends AnyFlatSpec with Matchers {
  "OSFile.constructFromString" should "return Dir for directory files" in {
    val result = OSFile.constructFromString("~/", "dir d").get
    result.name shouldEqual "d"
  }

  "OSFile.constructFromString" should "return Data File for Data files" in {
    val result = OSFile.constructFromString("~/", "8504156 c.dat").get
    result shouldEqual DataFile(name = "c.dat", path = "~/", size = 8504156)

  }

  "OsFile#isInPath" should "return true for files in the path" in {
    Directory(name = "haha", path = "/456/89/900/").isInPath("/") shouldEqual true
    Directory(name = "haha", path = "/456/89/900/").isInPath("/456/") shouldEqual true
    Directory(name = "haha", path = "/456/89/900/").isInPath("/456/89/900") shouldEqual true
  }
}
