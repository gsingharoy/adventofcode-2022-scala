package day8

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int] {
  override def part1(args: List[String]): Int = {
    // construct a forest which will contain trees with locations
    val forest = Forest.constructFromStrings(args)

    // iterate through each tree and check if it is visible to the edge and return a total number of such trees
    forest.trees.map(forest.isTreeVisibleToAnyOfTheEdges).count(_ == true)
  }

  override def part2(args: List[String]): Int = {
    // construct a forest which will contain trees with locations
    val forest = Forest.constructFromStrings(args)

    // iterate through each tree and check if it's scenic scores and pick the highest one
    forest.trees.map(forest.scenicScore).sortWith(_ > _).headOption.getOrElse(0)
  }
}
