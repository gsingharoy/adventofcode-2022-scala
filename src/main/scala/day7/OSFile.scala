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

  def constructFromString(currPath: OSPath, str: String): Option[OSFile] = str match {
      case s"dir ${dir}" => Some(Directory(path = currPath, name = dir))
      case s"${size} ${name}" => size.toIntOption.map(si=> DataFile(name = name,size = si, path = currPath))
      case _ => None
    }
}

case class DataFile(name: String,
                    size: Int,
                    path: OSPath) extends OSFile

case class Directory(name: String,
                     path: OSPath) extends OSFile
