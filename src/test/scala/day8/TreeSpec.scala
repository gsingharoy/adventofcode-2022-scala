package day8

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {

  "Tree#isVisibleToEdge" should "return true if the tree is in the edge" in {
    val tree1 = Tree(col = 0, row = 0, height = 9)
    tree1.isVisibleToEdge() shouldEqual true
  }

  "Tree#isVisibleToEdge" should "return true if all the trees in front are smaller" in {
    val tree1 = Tree(col = 0, row = 0, height = 5)
    val tree2 = Tree(col = 0, row = 0, height = 4)
    val tree3 = Tree(col = 0, row = 0, height = 2)
    val tree4 = Tree(col = 0, row = 0, height = 1)
    val tree5 = Tree(col = 0, row = 0, height = 0)
    val tree6 = Tree(col = 0, row = 0, height = 2)
    val tree7 = Tree(col = 0, row = 0, height = 3)

    tree1.isVisibleToEdge(List(tree2, tree3, tree4, tree5, tree6, tree7)) shouldEqual true
  }

  "Tree#isVisibleToEdge" should "return false there is at least one tree in the front which is taller or the sam size" in {
    val tree1 = Tree(col = 0, row = 0, height = 5)
    val tree2 = Tree(col = 0, row = 0, height = 4)
    val tree3 = Tree(col = 0, row = 0, height = 2)
    val tree4 = Tree(col = 0, row = 0, height = 5)
    val tree5 = Tree(col = 0, row = 0, height = 0)
    val tree6 = Tree(col = 0, row = 0, height = 2)
    val tree7 = Tree(col = 0, row = 0, height = 3)

    tree1.isVisibleToEdge(List(tree2, tree3, tree4, tree5, tree6, tree7)) shouldEqual false

    val tree8 = Tree(col = 0, row = 0, height = 6)

    tree1.isVisibleToEdge(List(tree8)) shouldEqual false
  }
}
