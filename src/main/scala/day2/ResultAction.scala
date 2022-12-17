package day2

trait ResultAction

object ResultAction {

  def constructAction(input: Char, config: ResultConfig): Option[ResultAction] = input.toUpper match {
    case config.win => Some(Win())
    case config.lose => Some(Lose())
    case config.draw => Some(Draw())
    case _ => None
  }
}

case class Win() extends  ResultAction

case class Lose() extends ResultAction

case class Draw() extends ResultAction


