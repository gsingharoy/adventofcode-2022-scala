package day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElfSpec extends AnyFlatSpec with Matchers {
  "Elf#totalCalories" should "return the sum of all calories it is carrying" in  {
    Elf(List.empty).totalCalories shouldEqual 0

    Elf(List(200, 4000, 78)).totalCalories shouldEqual 4278
  }
}
