package day2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameActionSpec extends AnyFlatSpec with Matchers {
  "GameAction#resultScore" should "result in correct scores when it is a draw" in {
    GameAction(Rock(), Rock()).resultScore shouldEqual 4
    GameAction(Paper(), Paper()).resultScore shouldEqual 5
    GameAction(Scissors(), Scissors()).resultScore shouldEqual 6
  }

  "GameAction.constructFromString" should "result in None for cases where two actions are not formed successfully" in {
    val config1 = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val config2 = PlayConfig(rock = 'X', paper = 'Y', scissors = 'Z')

    GameAction.constructFromString("L P", config1, config2) shouldEqual None

    GameAction.constructFromString("A P", config1, config2) shouldEqual None

    GameAction.constructFromString("L X", config1, config2) shouldEqual None

    GameAction.constructFromString("A X N", config1, config2) shouldEqual None
  }

  "GameAction.constructFromString" should "result in a success for cases where two actions are formed" in {
    val config1 = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val config2 = PlayConfig(rock = 'X', paper = 'Y', scissors = 'Z')

    GameAction.constructFromString("A X", config1, config2) shouldEqual Some(
      GameAction(Rock(), Rock())
    )

    GameAction.constructFromString("A Y", config1, config2) shouldEqual Some(
      GameAction(Rock(), Paper())
    )

    GameAction.constructFromString("A Z", config1, config2) shouldEqual Some(
      GameAction(Rock(), Scissors())
    )

    GameAction.constructFromString("B X", config1, config2) shouldEqual Some(
      GameAction(Paper(), Rock())
    )

    GameAction.constructFromString("C X", config1, config2) shouldEqual Some(
      GameAction(Scissors(), Rock())
    )
  }

  "GameAction.constructFromString" should "result in None for cases where the action and outcome are not formed successfully" in {
    val config1 = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val config2 = ResultConfig(win = 'Z', draw = 'Y', lose = 'X')

    GameAction.constructFromString("L P", config1, config2) shouldEqual None

    GameAction.constructFromString("A P", config1, config2) shouldEqual None

    GameAction.constructFromString("L X", config1, config2) shouldEqual None

    GameAction.constructFromString("A X N", config1, config2) shouldEqual None
  }

  "GameAction.constructFromString" should "result in a success for cases where action and result  are formed" in {
    val config1 = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val config2 = ResultConfig(win = 'Z', draw = 'Y', lose = 'X')

    GameAction.constructFromString("A X", config1, config2) shouldEqual Some(
      GameAction(Rock(), Scissors())
    )

    GameAction.constructFromString("A Y", config1, config2) shouldEqual Some(
      GameAction(Rock(), Rock())
    )

    GameAction.constructFromString("A Z", config1, config2) shouldEqual Some(
      GameAction(Rock(), Paper())
    )

    GameAction.constructFromString("B X", config1, config2) shouldEqual Some(
      GameAction(Paper(), Rock())
    )

    GameAction.constructFromString("C X", config1, config2) shouldEqual Some(
      GameAction(Scissors(), Paper())
    )
  }
}
