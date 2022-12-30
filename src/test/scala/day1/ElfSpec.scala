package day1

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElfSpec extends AnyFlatSpec with Matchers {
  "Elf#totalCalories" should "return the sum of all calories it is carrying" in {
    Elf(List[Int]()).totalCalories shouldEqual 0

    Elf(List(200, 4000, 78)).totalCalories shouldEqual 4278
  }

  "Elf.fromStrings" should "return an elf based on ONLY valid integers" in {
    Elf.fromStrings(List("haha", "", "89", "1", "io", "10")) shouldEqual Elf(List(89, 1, 10))
  }

  "Elf.constructElves" should "construct elves with empty characters as separators" in {
    val elf1 = Elf(List(90, 89, 90))
    val elf2 = Elf(List(199, 9))
    val elf3 = Elf(List(1, 2, 100))

    val elves = Elf.constructElves(List("90", "89", "90", "", "199", "9", "", "1", "2", "100"))
    elves shouldEqual List(elf1, elf2, elf3)
  }
}
