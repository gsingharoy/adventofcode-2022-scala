package day6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeviceSpec extends AnyFlatSpec with Matchers {

  "Device#startOfMarker" should "return the correct start of the marker" in {
    val d1 = Device("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
    d1.startOfMarker shouldBe Some(7)

    val d2 = Device("bvwbjplbgvbhsrlpgdmjqwftvncz")
    d2.startOfMarker shouldBe Some(5)

    val d3 = Device("nppdvjthqldpwncqszvftbrmjlhg")
    d3.startOfMarker shouldBe Some(6)

    val d4 = Device("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
    d4.startOfMarker shouldBe Some(10)

    val d5 = Device("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
    d5.startOfMarker shouldBe Some(11)
  }

}
