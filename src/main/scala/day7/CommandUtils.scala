package day7

import scala.annotation.tailrec

object CommandUtils {

  def changeDirectory(command: ChangeDirectory, currPath: OSPath): OSPath = command.cd match {
    case "/" => "/"
    case ".." => currPath.parentPath
    case cd => s"${currPath.path}$cd/"
  }

  /**
   *
   * @param directoryPath Pass the current path of the directory
   * @param allFiles All the files present in the directory
   * @return The total size of the directory
   */
  def directorySize(directory: Directory, allFiles: List[OSFile]): Int =
    allFiles
      .filter(_.isInPath(s"${directory.fullName}/"))
      .flatMap({
      case _: Directory => None
      case df: DataFile => Some(df.size)
    }
  ).sum

  def directoriesWithSizes(allFiles: List[OSFile]): List[(Int, Directory)] =
    allFiles.flatMap({
      case d: Directory => Some(d)
      case _ => None
    }).map(d => (directorySize(d, allFiles),d))

  def constructDirectoryList(strCommands: List[String]): List[OSFile] = {

    @tailrec
    def constructFiles(currPath: OSPath,
                       commands: List[String],
                       result: List[OSFile] = List.empty): (List[String], List[OSFile]) = {
      commands match {
        case Nil => (List.empty, result)
        case head :: tail if (isExecutableCommand(head)) => (head :: tail, result) // start of new commands and end of directory structure
        case head :: tail => OSFile.constructFromString(currPath, head) match {
          case Some(file) => {
            constructFiles(currPath, tail, result :+ file)
          }
        }
      }
    }

    @tailrec
    def executeCommands(currPath: OSPath,
                        commands: List[String],
                        result: List[OSFile] = List.empty): List[OSFile] = commands match {
      case Nil => result
      case head :: tail => ShellCommand.constructFromString(head) match {
          case Some(c) => c match {
            case _: ListDirectory => {
              // execute a listing of directory to find all the files in the directory
              val r = constructFiles(currPath, tail)
              executeCommands(currPath, r._1, result ++ r._2)
            }
            case cd: ChangeDirectory => executeCommands(changeDirectory(cd, currPath), tail, result) // execute a change in directory
          }
          case None => executeCommands(currPath, tail, result)
        }
    }

    executeCommands("/", strCommands)
  }
  private def isExecutableCommand(str: String): Boolean = str.substring(0,1) == "$"

}
