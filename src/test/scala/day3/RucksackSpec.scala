package day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RucksackSpec extends  AnyFlatSpec with Matchers {
  "Rucksack.constructFromString" should "return None when an empty string is passed" in {
    Rucksack.constructFromString("") shouldEqual None
  }

  "Rucksack.constructFromString" should "return None when an input is provided which is odd in length" in {
    Rucksack.constructFromString("hjUik") shouldEqual None
  }

  "Rucksack.constructFromString" should "return None when an input contains invalid characters" in {
    Rucksack.constructFromString("hjU8") shouldEqual None

    Rucksack.constructFromString("hjUjkl$g") shouldEqual None
  }

  "Rucksack.constructFromString" should "return a rucksack when a valid string in inputted" in {
    Rucksack.constructFromString("vJrwpWtwJgWrhcsFMMfFFhFp") shouldEqual Some(Rucksack("vJrwpWtwJgWr".toList, "hcsFMMfFFhFp".toList))
  }

  "Rucksack#duplicatedItems" should "return the duplicated items in them" in {
    Rucksack.constructFromString("vJrwpWtwJgWrhcsFMMfFFhFp").get.duplicatedItems shouldEqual List('p')

    Rucksack.constructFromString("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL").get.duplicatedItems shouldEqual List('L')

    Rucksack.constructFromString("PmmdzqPrVvPwwTWBwg").get.duplicatedItems shouldEqual List('P')

    Rucksack.constructFromString("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn").get.duplicatedItems shouldEqual List('v')

    Rucksack.constructFromString("ttgJtRGJQctTZtZT").get.duplicatedItems shouldEqual List('t')

    Rucksack.constructFromString("CrZsJsPPZsGzwwsLwLmpwMDw").get.duplicatedItems shouldEqual List('s')


  }
}
