package day7

case class DataFile(name: String,
                    size: Int,
                    path: OSPath) extends OSFile
