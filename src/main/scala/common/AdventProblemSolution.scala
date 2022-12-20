package common

trait AdventProblemSolution[X, Y] {

  def part1(args: List[String]): X

  def part2(args: List[String]): Y
}
