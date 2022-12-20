package day7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShellCommandSpec extends AnyFlatSpec with Matchers {
  "ShellCommand.constructFromString" should "return change directory command" in {
    ShellCommand.constructFromString("$ cd ..") shouldEqual Some(ChangeDirectory(".."))
  }

  "ShellCommand.constructFromString" should "return list directory command" in {
    ShellCommand.constructFromString("$ ls") shouldEqual Some(ListDirectory())
  }

  "ShellCommand.constructFromString" should "return None for ill formed strings" in {
    ShellCommand.constructFromString("$ cd i 78") shouldEqual None
  }
}
