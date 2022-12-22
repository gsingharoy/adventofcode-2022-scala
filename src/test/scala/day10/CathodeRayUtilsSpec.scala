package day10

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CathodeRayUtilsSpec extends AnyFlatSpec with Matchers {

  "CathodeRayUtils.makeOperations" should "successfully create all the operations" in {
    val args = FileUtils.readFile("day10/sample")

    val ops = Operation.fromStrings(args)

    val (resultX, resultCycles) = CathodeRayUtils.makeOperations(ops)

    CathodeRayUtils.findSignalStrength(20, resultCycles) shouldEqual 420

    CathodeRayUtils.findSignalStrength(60, resultCycles) shouldEqual 1140

    CathodeRayUtils.findSignalStrength(100, resultCycles) shouldEqual 1800

    CathodeRayUtils.findSignalStrength(140, resultCycles) shouldEqual 2940

    CathodeRayUtils.findSignalStrength(180, resultCycles) shouldEqual 2880

    CathodeRayUtils.findSignalStrength(220, resultCycles) shouldEqual 3960

  }
  
}
