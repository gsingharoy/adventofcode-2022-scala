package day7

import java.util.UUID
import scala.annotation.tailrec
import scala.util.Random

case class Directory(name: String,
                     path: OSPath) extends OSFile
