package solution

import common.FileUtils
import day7.CommandUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay7Spec extends AnyFlatSpec with Matchers {

  "Day 7: Part one" should "be able to find the directories with sizes more than 100k and sum them up" in {

    val strCommands = FileUtils.readFile("day7/input")
    val allFiles = CommandUtils.constructDirectoryList(strCommands)


    CommandUtils.directoriesWithSizes(allFiles).filter(_._1 <= 100000).map(_._1).sum shouldEqual 1453349
  }

}
