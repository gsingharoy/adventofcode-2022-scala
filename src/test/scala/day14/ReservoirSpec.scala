package day14

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReservoirSpec extends AnyFlatSpec with Matchers {
  "Reservoir.fromStrings" should "be able to build from the sample input" in {
    val args = FileUtils.readFile("day14/sample")

    // This print screen is just to show the screen
    Reservoir.fromStrings(args).mkString.map(println)

    Reservoir.fromStrings(args).pixels.length shouldEqual 20
  }

  "Reservoir.fromStrings" should "be able to build from the actual input" in {
    val args = FileUtils.readFile("day14/input")

    // This print screen is just to show the screen
    Reservoir.fromStrings(args).mkString.map(println)

  }
}
