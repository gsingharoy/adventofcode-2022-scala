package day9

import common.AdventProblemSolution

object Solution extends AdventProblemSolution[Int, Int] {
  override def part1(args: List[String]): Int = {

    // Build all the moves
    val moves: List[Move] = Move.fromStrings(args)

    val rb: RopeBridge = RopeBridgeUtils.makeMoves(moves)

    rb.uniqueTailPositions
  }

  override def part2(args: List[String]): Int = {

    // Build all the moves
    val moves: List[Move] = Move.fromStrings(args)

    val rb: RopeBridge = RopeBridgeUtils
      .makeMoves(moves, Head.StartPosition, Range.inclusive(1,9).toList.map( i=> Tail.StartPosition.copy(id = i))) // initialize a list of tails before making moves

    rb.uniqueTailPositions
  }
}
