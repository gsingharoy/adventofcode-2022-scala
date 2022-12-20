package day7

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int]{
  override def part1(args: List[String]): Int = {
    // construct all files
    val allFiles = CommandUtils.constructDirectoryList(args)
    // sum up sizes of all the directories
    CommandUtils.directoriesWithSizes(allFiles).filter(_._1 <= 100000).map(_._1).sum
  }

  override def part2(args: List[String]): Int = {
    val allFiles = CommandUtils.constructDirectoryList(args)

    val directorySizes = CommandUtils.directoriesWithSizes(allFiles)

    // total size of all the files which is occuppied
    val totalSizeOccupied = allFiles.map({
      case d: DataFile => d.size
      case _ => 0
    }).sum

    // return the minimum size of the directory to be deleted
    directorySizes
      .map(_._1)
      .filter(s => 70000000 - (totalSizeOccupied - s) >= 30000000)
      .sorted
      .headOption
      .getOrElse(totalSizeOccupied)

  }
}
