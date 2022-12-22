package day10

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OperationSpec extends AnyFlatSpec with Matchers {
  "Operation.fromString" should "be successfully able to form the operations" in {
    Operation.fromString("noop").get shouldEqual(Noop())

    Operation.fromString("addx -16").get shouldEqual(Addx(-16))
  }
  "Operation.fromString" should "return None for ill formed strings" in {
    Operation.fromString("noopo") shouldEqual None

    Operation.fromString("addx p") shouldEqual None
  }
}
