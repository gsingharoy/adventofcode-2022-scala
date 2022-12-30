package day15

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YCoordinateRangeSpec extends AnyFlatSpec with Matchers {
  "YCoordinateRange#split" should "return the same range when the coordinate is not within the range" in {

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
      .split(Coordinate(2, 101)) shouldEqual (List(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
    ))

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
      .split(Coordinate(-1, 100)) shouldEqual (List(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
    ))

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
      .split(Coordinate(201, 100)) shouldEqual (List(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
    ))
  }

  "YCoordinateRange#split" should "return the updated range if the split point is one of the edges of the range" in {

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
      .split(Coordinate(0, 100)) shouldEqual List(YCoordinateRange(y = 100, xStart = 1, xEnd = 200))

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
      .split(Coordinate(200, 100)) shouldEqual List(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 199)
    )
  }

  "YCoordinateRange#split" should "return the updated range if the split point is between the range" in {

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
      .split(Coordinate(10, 100)) shouldEqual List(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 9),
      YCoordinateRange(y = 100, xStart = 11, xEnd = 200)
    )

  }

  "CoordinateRange#merge" should "be able to return None which have no overlaps" in {
    YCoordinateRange(y = 100, xStart = 0, xEnd = 200).merge(
      YCoordinateRange(y = 100, xStart = 202, xEnd = 300)
    ) shouldEqual None

  }

  "CoordinateRange#merge" should "be able to return a single list when the lists are next to each other" in {
    YCoordinateRange(y = 100, xStart = 0, xEnd = 200).merge(
      YCoordinateRange(y = 100, xStart = 201, xEnd = 300)
    ) shouldEqual Some(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
    )

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200).merge(
      YCoordinateRange(y = 100, xStart = -10, xEnd = -1)
    ) shouldEqual Some(
      YCoordinateRange(y = 100, xStart = -10, xEnd = 200)
    )
  }

  "CoordinateRange#merge" should "be able to return a single list when the lists are overlapping" in {
    YCoordinateRange(y = 100, xStart = 0, xEnd = 200).merge(
      YCoordinateRange(y = 100, xStart = 199, xEnd = 300)
    ) shouldEqual Some(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
    )

    YCoordinateRange(y = 100, xStart = 0, xEnd = 200).merge(
      YCoordinateRange(y = 100, xStart = -10, xEnd = 20)
    ) shouldEqual Some(
      YCoordinateRange(y = 100, xStart = -10, xEnd = 200)
    )

    YCoordinateRange(y = 100, xStart = 199, xEnd = 300).merge(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
    ) shouldEqual Some(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
    )

    YCoordinateRange(y = 100, xStart = -10, xEnd = 20).merge(
      YCoordinateRange(y = 100, xStart = 0, xEnd = 200)
    ) shouldEqual Some(
      YCoordinateRange(y = 100, xStart = -10, xEnd = 200)
    )
  }

  "YCoordinateRange#applyLimit" should "be able to return the same value if it is completely in range" in {
    YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
      .applyLimit(-1, 301) shouldEqual Some(YCoordinateRange(y = 100, xStart = 0, xEnd = 300))
  }

  "YCoordinateRange#applyLimit" should "be able to return shaved range value if it is partially in limit" in {
    YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
      .applyLimit(2, 301) shouldEqual Some(YCoordinateRange(y = 100, xStart = 2, xEnd = 300))

    YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
      .applyLimit(-1, 200) shouldEqual Some(YCoordinateRange(y = 100, xStart = 0, xEnd = 200))
  }

  "YCoordinateRange#applyLimit" should "be able to return shaved range value if it is completely in limit" in {
    YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
      .applyLimit(10, 200) shouldEqual Some(YCoordinateRange(y = 100, xStart = 10, xEnd = 200))

  }

  "YCoordinateRange#applyLimit" should "be able to return None  value if it is not in limit" in {
    YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
      .applyLimit(301, 350) shouldEqual None

    YCoordinateRange(y = 100, xStart = 0, xEnd = 300)
      .applyLimit(-10, -1) shouldEqual None

  }

}
