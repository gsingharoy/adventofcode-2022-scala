package solution

import common.FileUtils
import day7.{CommandUtils, DataFile}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay7Spec extends AnyFlatSpec with Matchers {

  "Day 7: Part one" should "be able to find the directories with sizes more than 100k and sum them up" in {

    val strCommands = FileUtils.readFile("day7/input")
    val allFiles = CommandUtils.constructDirectoryList(strCommands)


    CommandUtils.directoriesWithSizes(allFiles).filter(_._1 <= 100000).map(_._1).sum shouldEqual 1453349
  }

  "Day 7: part two" should "be able to find the smallest file to delete and free up the space" in {
    val strCommands = FileUtils.readFile("day7/input")
    val allFiles = CommandUtils.constructDirectoryList(strCommands)

    val directorySizes = CommandUtils.directoriesWithSizes(allFiles)
    val totalSizeOccupied = allFiles.map({
        case d:DataFile  => d.size
        case _ => 0
      }).sum
    val unusedSpace = 70000000 - totalSizeOccupied
    val minSizeToBeDeleted = directorySizes
      .map(_._1)
      .filter(s => 70000000 - (totalSizeOccupied - s) >= 30000000)
      .sorted
      .headOption
      .getOrElse(totalSizeOccupied)


    minSizeToBeDeleted shouldEqual 2948823
  }

}
