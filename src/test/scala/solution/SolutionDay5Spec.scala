package solution

import common.FileUtils
import day5.{CrateStack, CrateStackUtils, MoveAction}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class SolutionDay5Spec extends AnyFlatSpec with Matchers {

  "Day 5 : Part one" should "be able to find the top most crates in each stack" in {
    val allLines = FileUtils.readFile("day5/input")

    @tailrec
    def f(lines: List[String], result: (List[String])): (List[String], List[String]) = lines match {
      case Nil => (result, List.empty)
      case head :: tail if (head.isEmpty) => (result, tail)
      case head :: tail => f(tail, (result :+ head))
    }

    val (crateLines, moveLines) = f(allLines, List.empty)

    val crateStacks = CrateStack.constructCrateStacks(crateLines)
    val moves: List[MoveAction] = moveLines.flatMap(MoveAction.constructFromString)

    val result = CrateStackUtils.makeMoves(moves, crateStacks)

    result._1 shouldEqual 502
    CrateStackUtils.topCrates(result._2) shouldEqual "SPFMVDTZT"

  }

  "Day 5 : Part two" should "be able to find the top most crates in each stack when using an advanced 9001 crane" in {
    val allLines = FileUtils.readFile("day5/input")

    @tailrec
    def f(lines: List[String], result: (List[String])): (List[String], List[String]) = lines match {
      case Nil => (result, List.empty)
      case head :: tail if (head.isEmpty) => (result, tail)
      case head :: tail => f(tail, (result :+ head))
    }

    val (crateLines, moveLines) = f(allLines, List.empty)

    val crateStacks = CrateStack.constructCrateStacks(crateLines)
    val moves: List[MoveAction] = moveLines.flatMap(MoveAction.constructFromString)

    val result = CrateStackUtils.makeMoves(moves, crateStacks, with9001 = true)

    result._1 shouldEqual 502
    CrateStackUtils.topCrates(result._2) shouldEqual "ZFSJBPRFP"

  }

}
