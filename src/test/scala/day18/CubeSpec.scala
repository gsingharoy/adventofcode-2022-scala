package day18

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CubeSpec extends AnyFlatSpec with Matchers {
  "Cube.fromStr" should "be able to read a cube successully" in {
    Cube.fromString("1,2,7") shouldEqual Some(Cube((1, 2, 7)))
  }

  "Cube#hasAdjacentSides" should "be able to return the correct result" in {
    Cube((1, 1, 1)).hasAdjacentSides(Cube((2, 1, 1))) shouldEqual true

    Cube((1, 1, 1)).hasAdjacentSides(Cube((4, 1, 1))) shouldEqual false
  }
}
