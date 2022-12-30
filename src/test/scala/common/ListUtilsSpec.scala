package common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListUtilsSpec extends AnyFlatSpec with Matchers {
  "ListUtils.zipListByPivotValue" should "return a list of lists based on the pivot value" in {
    val args1 = List("aaa", "1", "bb", "cc", "89", "1", "90")
    ListUtils.zipListByPivotValue(args1, "1") shouldEqual List(
      List("aaa"),
      List("bb", "cc", "89"),
      List("90")
    )
  }
}
