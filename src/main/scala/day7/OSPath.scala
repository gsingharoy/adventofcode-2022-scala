package day7

import scala.language.implicitConversions

case class OSPath(path: String) {

  /** Returns the parent path
    */
  lazy val parentPath: String = path match {
    case "/" => "/"
    case cp  => cp.split("/").toList.dropRight(1).mkString("/").concat("/")
  }

}

object OSPath {
  implicit def fromString(str: String): OSPath = OSPath(str)
}
