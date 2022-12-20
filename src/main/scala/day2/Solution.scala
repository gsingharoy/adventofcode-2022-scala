package day2

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int]{

  override def part1(args: List[String]): Int = {
    val opponentConfig = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val yourConfig = PlayConfig(rock = 'X', paper = 'Y', scissors = 'Z')

    val actions: List[GameAction] = args
      .flatMap(GameAction.constructFromString(_, opponentConfig = opponentConfig, yourConfig = yourConfig))

    actions.map(_.resultScore).sum
  }

  override def part2(args: List[String]): Int = {
    val opponentConfig = PlayConfig(rock = 'A', paper = 'B', scissors = 'C')
    val yourResultConfig = ResultConfig(win = 'Z', lose = 'X', draw = 'Y')

    val actions = args.flatMap(
      GameAction.constructFromString(_, opponentConfig = opponentConfig, yourResultConfig = yourResultConfig)
    )
    actions.map(_.resultScore).sum
  }
}
