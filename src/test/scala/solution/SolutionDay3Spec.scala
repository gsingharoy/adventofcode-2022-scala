package solution

import common.FileUtils
import day3.{Rucksack, RucksackUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay3Spec extends AnyFlatSpec with Matchers {

  "Solution Day 3: Part one" should "be able find the correct total of the scores" in {
    val inputStrings = FileUtils.readFile("day3/input")

    val ruckSacks: List[Rucksack] = inputStrings.flatMap(Rucksack.constructFromString)

    val totalScore: Int = ruckSacks.map(r => r.duplicatedItems.map(RucksackUtils.itemScore).sum).sum

    totalScore shouldEqual 7863
  }
}
