package day11

import common.{AdventProblemSolution, ListUtils}

object Solution extends AdventProblemSolution[BigInt, BigInt] {

  override def part1(args: List[String]): BigInt = {
    val zippedArgs = ListUtils.zipListByPivotValue(args, "")
    // parse and form all the monkeys
    val monkeys: List[Monkey] = zippedArgs.flatMap(Monkey.fromStrings(_))

    MonkeyUtils.calculateMoneeyBusiness(monkeys, 20).getOrElse(-1)
  }

  override def part2(args: List[String]): BigInt = {
    val zippedArgs = ListUtils.zipListByPivotValue(args, "")
    // parse and form all the monkeys
    val monkeys: List[Monkey] = zippedArgs.flatMap(Monkey.fromStrings(_, true))

    MonkeyUtils.calculateMoneeyBusiness(monkeys, 10000, true).getOrElse(-1)
  }
}
