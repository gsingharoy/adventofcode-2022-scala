package day7

import java.util.UUID

case class DataFile(name: String,
                    size: Int,
                    path: OSPath) extends OSFile
