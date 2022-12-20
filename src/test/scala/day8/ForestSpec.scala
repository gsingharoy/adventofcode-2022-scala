package day8

import common.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForestSpec extends AnyFlatSpec with Matchers  {
  "Forest.constructFromStrings" should "be able to construct the tree from a sample example" in {
    val args = FileUtils.readFile("day8/sample")

    val forest = Forest.constructFromStrings(args)

    forest.trees.length shouldBe 25
  }

  "Forest#isTreeVisibleToAnyOfTheEdges" should "return true for the trees in the edges" in {
    val args = FileUtils.readFile("day8/sample")

    val forest = Forest.constructFromStrings(args)


    val t00: Tree = forest.trees.find(t => t.col == 0 && t.row == 0).get
    val t44: Tree = forest.trees.find(t => t.col == 4 && t.row == 4).get
    val t14: Tree = forest.trees.find(t => t.col == 4 && t.row == 1).get

    forest.isTreeVisibleToAnyOfTheEdges(t00) shouldBe true
    forest.isTreeVisibleToAnyOfTheEdges(t44) shouldBe true
    forest.isTreeVisibleToAnyOfTheEdges(t14) shouldBe true
  }

  "Forest#isTreeVisibleToAnyOfTheEdges" should "return true for the trees which are visible in certain sides but are in the middle" in {
    val args = FileUtils.readFile("day8/sample")

    val forest = Forest.constructFromStrings(args)


    val t11: Tree = forest.trees.find(t => t.col == 1 && t.row == 1).get
    val t12: Tree = forest.trees.find(t => t.col == 2 && t.row == 1).get

    forest.isTreeVisibleToAnyOfTheEdges(t11) shouldBe true
    forest.isTreeVisibleFromLeft(t11) shouldBe true
    forest.isTreeVisibleFromNorth(t11) shouldBe true
    forest.isTreeVisibleFromSouth(t11) shouldBe false
    forest.isTreeVisibleFromRight(t11) shouldBe false

    forest.isTreeVisibleToAnyOfTheEdges(t12) shouldBe true
    forest.isTreeVisibleFromLeft(t12) shouldBe false
    forest.isTreeVisibleFromNorth(t12) shouldBe true
    forest.isTreeVisibleFromSouth(t12) shouldBe false
    forest.isTreeVisibleFromRight(t12) shouldBe true
  }
}
