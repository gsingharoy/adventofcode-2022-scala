package day7

trait ShellCommand

object ShellCommand {

  def constructFromString(str: String): Option[ShellCommand] =
    str match {
    case s"$$ ls" => Some(ListDirectory())
    case s"$$ cd ${dir}" if (dir.split(" ").length == 1) => Some(ChangeDirectory(dir)) // pattern matched with a change directory
    case _ => None
  }
}

case class ChangeDirectory(cd: String) extends ShellCommand

case class ListDirectory() extends ShellCommand
