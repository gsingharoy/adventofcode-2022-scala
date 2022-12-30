package day14

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PixelSpec extends AnyFlatSpec with Matchers {

  "Position.drawLine" should "be able to draw the lines successfully" in {

    // line to the top

    Position(10, 10).drawLine(Position(10, 13)) shouldEqual List(
      Position(10, 11),
      Position(10, 12),
      Position(10, 13)
    )

    // line to the bottom
    val result2 = Position(10, 10).drawLine(Position(10, 6))
    result2.length shouldEqual 4
    result2.contains(Position(10, 9)) shouldEqual true
    result2.contains(Position(10, 8)) shouldEqual true
    result2.contains(Position(10, 7)) shouldEqual true
    result2.contains(Position(10, 6)) shouldEqual true

    // line to the right
    val result3 = Position(10, 10).drawLine(Position(13, 10))
    result3.length shouldEqual 3
    result3.contains(Position(11, 10)) shouldEqual true
    result3.contains(Position(12, 10)) shouldEqual true
    result3.contains(Position(13, 10)) shouldEqual true

    // line to the left
    val result4 = Position(10, 10).drawLine(Position(7, 10))
    result4.length shouldEqual 3
    result4.contains(Position(9, 10)) shouldEqual true
    result4.contains(Position(8, 10)) shouldEqual true
    result4.contains(Position(7, 10)) shouldEqual true

  }

  "Brick.fromString" should "draw the bricks in expected order" in {
    Brick.fromString("498,4 -> 498,6 -> 496,6").sorted shouldEqual List(
      Brick(Position(496, 6)),
      Brick(Position(497, 6)),
      Brick(Position(498, 4)),
      Brick(Position(498, 5)),
      Brick(Position(498, 6))
    )

  }

}
