package day9

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeBridgeUtilsSpec extends AnyFlatSpec with Matchers {
  "RopeBridgeUtils.isTailNeededToMove" should "return false for the cases where head and tail are adjacent" in {
    val tail1 = Tail(Position(2, 4))
    val head1 = Head(Position(1, 4))

    RopeBridgeUtils.isTailNeededToMove(tail1, head1) shouldEqual false

    val tail2 = Tail(Position(4, 3))
    val head2 = Head(Position(3, 4))

    RopeBridgeUtils.isTailNeededToMove(tail2, head2) shouldEqual false

    val tail3 = Tail(Position(4, 3))
    val head3 = Head(Position(4, 2))

    RopeBridgeUtils.isTailNeededToMove(tail3, head3) shouldEqual false

    val tail4 = Tail(Position(4, 3))
    val head4 = Head(Position(4, 3))

    RopeBridgeUtils.isTailNeededToMove(tail4, head4) shouldEqual false
  }

  "RopeBridgeUtils.isTailNeededToMove" should "return true for the cases where head and tail are not adjacent" in {
    val tail1 = Tail(Position(0, 0))
    val head1 = Head(Position(2, 0))

    RopeBridgeUtils.isTailNeededToMove(tail1, head1) shouldEqual true

    val tail2 = Tail(Position(4, 1))
    val head2 = Head(Position(4, 3))

    RopeBridgeUtils.isTailNeededToMove(tail2, head2) shouldEqual true

    val tail3 = Tail(Position(4, 2))
    val head3 = Head(Position(3, 4))

    RopeBridgeUtils.isTailNeededToMove(tail3, head3) shouldEqual true
  }

  "RopeBridgeUtils.adjustTail" should "return the same tail value when it is adjacent" in {
    val tail1 = Tail(Position(2, 4))
    val head1 = Head(Position(1, 4))

    RopeBridgeUtils.adjustTail(tail1, head1) shouldEqual tail1

    val tail2 = Tail(Position(4, 3))
    val head2 = Head(Position(3, 4))

    RopeBridgeUtils.adjustTail(tail2, head2) shouldEqual tail2
  }

  "RopeBridgeUtils.isTailNeededToMove" should "adjust the tail accordingly" in {
    val tail1 = Tail(Position(0, 0))
    val head1 = Head(Position(2, 0))

    RopeBridgeUtils.adjustTail(tail1, head1) shouldEqual Tail(Position(1, 0))

    val tail2 = Tail(Position(4, 1))
    val head2 = Head(Position(4, 3))

    RopeBridgeUtils.adjustTail(tail2, head2) shouldEqual Tail(Position(4, 2))

    val tail3 = Tail(Position(3, 0))
    val head3 = Head(Position(4, 3))

    RopeBridgeUtils.adjustTail(tail3, head3) shouldEqual Tail(Position(4, 1))
  }

  "RopeBridgeUtils.makeMoves" should "be able to make the moves from the sample in the question" in {
    val args = FileUtils.readFile("day9/sample")

    val moves: List[Move] = Move.fromStrings(args)

    val rb: RopeBridge = RopeBridgeUtils.makeMoves(moves)

    rb.uniqueTailPositions shouldBe 13
  }

  "RopeBridgeUtils.makeMoves" should "be able to make the moves from the 2nd sample in the question" in {
    val args = FileUtils.readFile("day9/sample2")

    val moves: List[Move] = Move.fromStrings(args)

    val rb: RopeBridge = RopeBridgeUtils
      .makeMoves(
        moves,
        Head.StartPosition,
        Range.inclusive(1, 9).toList.map(i => Tail.StartPosition.copy(id = i))
      )

    rb.uniqueTailPositions shouldBe 36
  }
}
