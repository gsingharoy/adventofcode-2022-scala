package day9

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MoveSpec extends AnyFlatSpec with Matchers {

  "Move.fromString" should "be able to construct up parameters" in {
    Move.fromString("U 2") shouldEqual List(MoveUp(), MoveUp())
  }

  "Move.fromString" should "be able to construct Down parameters" in {
    Move.fromString("D 3") shouldEqual List(MoveDown(), MoveDown(), MoveDown())
  }

  "Move.fromString" should "be able to construct Left parameters" in {
    Move.fromString("L 1") shouldEqual List(MoveLeft())
  }

  "Move.fromString" should "be able to construct Right parameters" in {
    Move.fromString("R 2") shouldEqual List(MoveRight(), MoveRight())
  }

  "Move.fromString" should "Return Empty List for ill formed strings" in {
    Move.fromString("R u") shouldEqual List.empty
    Move.fromString("F 19") shouldEqual List.empty
    Move.fromString("U 19 9") shouldEqual List.empty
  }
}
