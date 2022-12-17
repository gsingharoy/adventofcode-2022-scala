package day2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResultActionSpec extends  AnyFlatSpec with Matchers {
  "PlayConfig.constructAction" should "return None if it cannot construct" in {
    val config = ResultConfig(win = 'A', lose = 'B', draw = 'C')

    ResultAction.constructAction('X', config) shouldEqual None

  }

  "PlayAction.constructAction" should "return Some(Action) if it identifies a correct action" in {
    val config = ResultConfig(win = 'A', lose = 'B', draw = 'C')

    ResultAction.constructAction('A', config) shouldEqual Some(Win())
    ResultAction.constructAction('B', config) shouldEqual Some(Lose())
    ResultAction.constructAction('C', config) shouldEqual Some(Draw())
  }
}
