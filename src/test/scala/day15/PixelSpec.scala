package day15

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PixelSpec extends AnyFlatSpec with Matchers {
  "Coordinate#manhattanDistance" should "able to return the correct Manhattan distance" in {
    Coordinate(1, 1).manhattanDistance(Coordinate(2, 2)) shouldEqual 2

    Coordinate(1, 1).manhattanDistance(Coordinate(2, -2)) shouldEqual 4

    Coordinate(-1, 1).manhattanDistance(Coordinate(2, -2)) shouldEqual 6

    Coordinate(-7, -1).manhattanDistance(Coordinate(8, -7)) shouldEqual 21
  }
}
