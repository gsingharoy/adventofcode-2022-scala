package day2

/** Defines an action of a game where it specifies the simultaneous turns being played
  *
  * @param opponentTurn
  * @param yourTurn
  */
case class GameAction(opponentTurn: PlayAction, yourTurn: PlayAction) {

  lazy val resultScore: Int = {
    (opponentTurn, yourTurn) match {
      case (_: Rock, _: Rock) | (_: Paper, _: Paper) | (_: Scissors, _: Scissors) => 3
      case (_: Rock, _: Paper) | (_: Paper, _: Scissors) | (_: Scissors, _: Rock) => 6
      case _                                                                      => 0
    }
  } + yourTurn.score
}

object GameAction {

  /** Reverse engineer an action based on the expected result
    *
    * @param opponentTurn
    * @param yourResult
    * @return
    */
  def reverseEngineer(opponentTurn: PlayAction, yourResult: ResultAction): GameAction =
    (opponentTurn, yourResult) match {
      case (a: Rock, _: Win)      => GameAction(a, Paper())
      case (a: Rock, _: Lose)     => GameAction(a, Scissors())
      case (a: Rock, _: Draw)     => GameAction(a, Rock())
      case (a: Paper, _: Win)     => GameAction(a, Scissors())
      case (a: Paper, _: Lose)    => GameAction(a, Rock())
      case (a: Paper, _: Draw)    => GameAction(a, Paper())
      case (a: Scissors, _: Win)  => GameAction(a, Rock())
      case (a: Scissors, _: Lose) => GameAction(a, Paper())
      case (a: Scissors, _: Draw) => GameAction(a, Scissors())
    }

  /** Construct an action from a raw string of the format "A X". Here A represents the opponent's
    * action and X represents your action. The config of the action needs to be mapped with the
    * input configs.
    *
    * @param i
    * @param opponentConfig
    * @param yourConfig
    * @return
    */
  def constructFromString(
      i: String,
      opponentConfig: PlayConfig,
      yourConfig: PlayConfig
  ): Option[GameAction] =
    safeStringMatch[PlayConfig](i, opponentConfig, yourConfig)((i, oc, yc) =>
      (
        PlayAction.constructAction(i.charAt(0), oc),
        PlayAction.constructAction(i.charAt(2), yc)
      ) match {
        case (Some(o), Some(y)) => Some(GameAction(o, y))
        case _                  => None
      }
    )

  /** Construct an action from a raw string of the format "A X". Here A represents the opponent's
    * action and X represents your expected result. The config of the action needs to be mapped with
    * the input configs.
    *
    * @param i
    * @param opponentConfig
    * @param yourResultConfig
    * @return
    */
  def constructFromString(
      i: String,
      opponentConfig: PlayConfig,
      yourResultConfig: ResultConfig
  ): Option[GameAction] =
    safeStringMatch[ResultConfig](i, opponentConfig, yourResultConfig)((i, oc, yc) =>
      (
        PlayAction.constructAction(i.charAt(0), oc),
        ResultAction.constructAction(i.charAt(2), yc)
      ) match {
        case (Some(o), Some(r)) => Some(GameAction.reverseEngineer(o, r))
        case _                  => None
      }
    )

  private def safeStringMatch[Y](i: String, opponentConfig: PlayConfig, yourConfig: Y)(
      f: (String, PlayConfig, Y) => Option[GameAction]
  ): Option[GameAction] =
    if (i.length != 3) None
    else f(i, opponentConfig, yourConfig)

}
