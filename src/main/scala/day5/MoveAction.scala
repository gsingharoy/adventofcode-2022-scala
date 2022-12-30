package day5

/** Represents a move action where certain crates are moved from one stack to another.
  *
  * @param sourceStack
  *   identifier of the crate stack source from where crates have to be moved
  * @param sinkStack
  *   Sink or the end stack to which the crates would be moved
  * @param moveUnits
  *   Units of crates to be moved from source to sink
  */
case class MoveAction(sourceStack: Int, sinkStack: Int, moveUnits: Int)

object MoveAction {

  /** Construct move Action from a string. If an ill formed string is passed None would be returned.
    * @param str
    *   should be of the format "move 1 from 2 to 1"
    *
    * @return
    */
  def constructFromString(str: String): Option[MoveAction] = str match {
    case s"move ${u} from ${so} to ${si}" =>
      (so.toIntOption, si.toIntOption, u.toIntOption) match {
        case (Some(source), Some(sink), Some(units)) =>
          Some(MoveAction(sourceStack = source, sinkStack = sink, moveUnits = units))
        case _ => None
      }
    case _ => None
  }
}
