package day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RucksackGroupSpec extends  AnyFlatSpec with Matchers {
  "RucksackGroup#duplicatedItems" should "return the expected duplicated items in three ruck sacks" in {
    val rucksack1: Rucksack = Rucksack.constructFromString("vJrwpWtwJgWrhcsFMMfFFhFp").get
    val rucksack2: Rucksack = Rucksack.constructFromString("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL").get
    val rucksack3: Rucksack = Rucksack.constructFromString("PmmdzqPrVvPwwTWBwg").get

    val rg = RucksackGroup(rucksack1, rucksack2, rucksack3)

    rg.duplicatedItems shouldEqual List('r')

  }

  "RucksackGroup#badge" should "return the expected badge which has the highest priority" in {
    val rucksack1: Rucksack = Rucksack.constructFromString("vJrwpWtwJgWrhcsFMMfFFhFp").get
    val rucksack2: Rucksack = Rucksack.constructFromString("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL").get
    val rucksack3: Rucksack = Rucksack.constructFromString("PmmdzqPrVvPwwTWBwg").get

    val rg = RucksackGroup(rucksack1, rucksack2, rucksack3)

    rg.badge shouldEqual Some('r')

  }
}
