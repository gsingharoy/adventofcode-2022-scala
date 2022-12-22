package day10

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Long, Unit] {
  override def part1(args: List[String]): Long = {
    val ops = Operation.fromStrings(args)

    val (_, resultCycles) = CathodeRayUtils.makeOperations(ops)

    List(20, 60, 100, 140, 180, 220).map(CathodeRayUtils.findSignalStrength(_, resultCycles)).sum
  }

  override def part2(args: List[String]): Unit = {
    val ops = Operation.fromStrings(args)

    val (_, resultCycles) = CathodeRayUtils.makeOperations(ops)

    // Print the strings
    CathodeRayUtils.buildPixels(resultCycles).foreach(println)
  }
}
