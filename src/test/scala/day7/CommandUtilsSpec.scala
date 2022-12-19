package day7

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandUtilsSpec extends AnyFlatSpec with Matchers {
  "CommandUtils.changeDirectory" should "return successfully to parent path" in {
    CommandUtils.changeDirectory(ChangeDirectory(".."), "/") shouldEqual OSPath("/")
    CommandUtils.changeDirectory(ChangeDirectory(".."), "/habanero/jklp/jkl/ooops/lala/") shouldEqual OSPath("/habanero/jklp/jkl/ooops/")
  }

  "CommandUtils.changeDirectory" should "change directory to a new one" in {
    CommandUtils.changeDirectory(ChangeDirectory("dk"), "/habanero/jklp/jkl/ooops/lala/") shouldEqual OSPath("/habanero/jklp/jkl/ooops/lala/dk/")
  }

  "CommandUtils.directoriesWithSizes" should "be able to find the directories with at max values" in {
    val strCommands = FileUtils.readFile("day7/sample")
    val allFiles = CommandUtils.constructDirectoryList(strCommands)


    CommandUtils.directoriesWithSizes(allFiles).filter(_._1 <= 100000).map(_._1).sum shouldEqual(95437)
  }
}
