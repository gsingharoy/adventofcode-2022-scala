package solution

import common.FileUtils
import day2.{GameAction, PlayConfig, ResultConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionDay2Spec extends  AnyFlatSpec with Matchers{

  "Solution 2: Part 1" should "be able to find the correct score based on the available strategy" in {
    val opponentConfig = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val yourConfig = PlayConfig(rock = 'X', paper = 'Y', scissors = 'Z')

    val actions: List[GameAction] = FileUtils
      .readFile("day2/input")
      .flatMap(GameAction.constructFromString(_, opponentConfig = opponentConfig, yourConfig = yourConfig))

    actions.map(_.resultScore).sum shouldEqual 15572
  }

  "Solution 2: Part 2" should "be able to find the correct score based on the available strategy for the bonus question" in {
    val opponentConfig = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val yourResultConfig = ResultConfig(win = 'Z', lose = 'X', draw = 'Y')

    val actions: List[GameAction] = FileUtils
      .readFile("day2/input")
      .flatMap(GameAction.constructFromString(_, opponentConfig = opponentConfig, yourResultConfig = yourResultConfig))

    actions.map(_.resultScore).sum shouldEqual 16098
  }

}
