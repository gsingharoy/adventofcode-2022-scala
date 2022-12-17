package day2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlayActionSpec extends  AnyFlatSpec with Matchers {
  "PlayAction.constructAction" should "return None if it cannot construct" in {
    val config = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')

    PlayAction.constructAction('X', config) shouldEqual None

  }

  "PlayAction.constructAction" should "return Some(Action) if it identifies a correct action" in {
    val config = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')

    PlayAction.constructAction('A', config) shouldEqual Some(Rock())
    PlayAction.constructAction('B', config) shouldEqual Some(Paper())
    PlayAction.constructAction('C', config) shouldEqual Some(Scissors())
  }
}
