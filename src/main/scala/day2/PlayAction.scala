package day2

trait PlayAction {

  def score: Int
}

object PlayAction {

  def constructAction(input: Char, config: PlayConfig): Option[PlayAction] = input.toUpper match {
    case config.rock     => Some(Rock())
    case config.paper    => Some(Paper())
    case config.scissors => Some(Scissors())
    case _               => None
  }

}

case class Rock() extends PlayAction {
  lazy val score: Int = 1
}

case class Paper() extends PlayAction {
  lazy val score: Int = 2
}

case class Scissors() extends PlayAction {
  lazy val score: Int = 3
}
