package day11

import common.{AdventProblemSolution, ListUtils}

object Solution extends AdventProblemSolution[Int, Int]{

  override def part1(args: List[String]): Int = {
    val zippedArgs = ListUtils.zipListByPivotValue(args, "")
    // parse and form all the monkeys
    val monkeys: List[Monkey] = zippedArgs.flatMap(Monkey.fromStrings)

    MonkeyUtils.calculateMoneeyBusiness(monkeys, 20).getOrElse(-1)
  }

  override def part2(args: List[String]): Int = ???
}
