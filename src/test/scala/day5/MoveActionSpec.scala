package day5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MoveActionSpec extends AnyFlatSpec with Matchers {
  "MoveAction.constructFromString" should "return None for ill formed strings" in {
    MoveAction.constructFromString("hjks ljlj") shouldEqual None

    MoveAction.constructFromString("move 1 from 2 to 1 and 7") shouldEqual None

    MoveAction.constructFromString("move 1 from 2 to a") shouldEqual None
  }

  "MoveAction.constructFromString" should "return correct Move action for correctly formed move" in {
    MoveAction.constructFromString("move 1 from 2 to 3").get shouldEqual MoveAction(
      moveUnits = 1,
      sourceStack = 2,
      sinkStack = 3
    )
  }
}
