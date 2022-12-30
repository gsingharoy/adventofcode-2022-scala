package day10

trait Operation {
  def cycles: Int

  def exec(totalCycles: Int, x: Int): (Int, List[CycleHistory])
}

object Operation {

  def fromString(str: String): Option[Operation] = str match {
    case s"noop"         => Some(Noop())
    case s"addx ${sInt}" => sInt.toIntOption.map(Addx)
    case _               => None
  }

  def fromStrings(strings: List[String]): List[Operation] = strings.flatMap(fromString)
}

case class Noop() extends Operation {
  lazy val cycles: Int = 1

  override def exec(totalCycles: Int, x: Int): (Int, List[CycleHistory]) = {
    val cycleHistories = List(
      CycleHistory(
        cycle = totalCycles + 1,
        startX = x,
        endX = x
      )
    )
    (x, cycleHistories)
  }
}

case class Addx(literal: Int) extends Operation {
  lazy val cycles: Int = 2

  override def exec(totalCycles: Int, x: Int): (Int, List[CycleHistory]) = {
    val cycleHistories = List(
      CycleHistory(
        cycle = totalCycles + 1,
        startX = x,
        endX = x
      ),
      CycleHistory(
        cycle = totalCycles + 2,
        startX = x,
        endX = x + literal
      )
    )
    (x + literal, cycleHistories)
  }
}

case class CycleHistory(cycle: Int, startX: Int, endX: Int)
