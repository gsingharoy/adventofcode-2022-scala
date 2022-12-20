package day7

trait OSFile {

  def name: String
  def path: OSPath

  def fullName: String =  s"${path.path}${name}"

  def isInRoot: Boolean = path.path == "/"

  def isInPath(rootPath: OSPath): Boolean = rootPath.path.length <= path.path.length &&
    path.path.substring(0, rootPath.path.length) == rootPath.path
}

object OSFile {

  def constructFromString(currPath: OSPath, str: String): Option[OSFile] = str.split(" ").toList match {
    case l if l.length != 2 => None
    case head1 :: head2 :: _ => head1 match {
      case "dir" => Some(Directory(path = currPath, name = head2))
      case h => h.toIntOption.map(i => DataFile(name = head2,size = i, path = currPath))
    }
  }
}
