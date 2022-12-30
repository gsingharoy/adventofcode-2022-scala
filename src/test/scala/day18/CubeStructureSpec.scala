package day18

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CubeStructureSpec extends AnyFlatSpec with Matchers {
  "CubeStructure#exposedSides" should "return the correct values based on the input" in {
    CubeStructure(List(Cube((1, 1, 1)), Cube((2, 1, 1)))).exposedSides shouldBe 10

  }
}
