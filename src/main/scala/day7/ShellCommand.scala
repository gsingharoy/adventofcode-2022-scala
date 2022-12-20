package day7

trait ShellCommand

object ShellCommand {

  def constructFromString(str: String): Option[ShellCommand] =
    str.drop(2).split(" ").toList match {
    case head :: Nil if (head == "ls") => Some(ListDirectory())
    case l if l.length > 2 => None
    case _ :: head2 :: _ => Some(ChangeDirectory(head2)) // pattern matched with a change directory
  }
}

case class ChangeDirectory(cd: String) extends ShellCommand

case class ListDirectory() extends ShellCommand
