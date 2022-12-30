package day1

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElfCalorieFinderSpec extends AnyFlatSpec with Matchers {
  "ElfCalorieFinder.findElfWithMaxCalories" should "return None when an empty list is passed" in {
    ElfCalorieFinder.findElfWithMaxCalories(List.empty) shouldEqual None
  }

  "ElfCalorieFinder.findElfWithMaxCalories" should "return the elf with the maximum calories when the list of elves is passed" in {
    val elf1 = Elf(List(500, 800))
    val elf2 = Elf(List(50, 800, 1000, 12000))
    val elf3 = Elf(List(5000, 8000))
    val elf4 = Elf(List(50, 80, 100, 70))

    val elves: List[Elf] = List[Elf](elf1, elf2, elf3, elf4)

    ElfCalorieFinder.findElfWithMaxCalories(elves) shouldEqual Some(elf2)
  }

  "ElfCalorieFinder.findTopElvesWithCalorificValues" should "return the maximum calorific elves" in {
    val elf1 = Elf(List(500, 800))
    val elf2 = Elf(List(50, 800, 1000, 12000))
    val elf3 = Elf(List(5000, 8000))
    val elf4 = Elf(List(50, 80, 100, 70))

    val elves: List[Elf] = List[Elf](elf1, elf2, elf3, elf4)

    ElfCalorieFinder.findTopElvesWithCalorificValues(elves, 2) shouldEqual List(elf2, elf3)
  }
}
