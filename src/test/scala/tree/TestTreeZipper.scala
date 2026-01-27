package proofPlayground
package tree

import org.scalatest.funsuite.AnyFunSuite

class TestTreeZipper extends AnyFunSuite:

  test("down then up restores the original tree") {
    val tree   = Tree.Node("A", List(Tree.Leaf("B"), Tree.Leaf("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.up) === Some(zipper))
  }

  test("down goes to left-most child") {
    val tree   = Tree.Node("A", List(Tree.Leaf("B"), Tree.Leaf("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.map(_.get) === Some(Tree.Leaf("B")))
  }

  test("right goes to the next sibling") {
    val tree   = Tree.Node("A", List(Tree.Leaf("B"), Tree.Leaf("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.right).map(_.get) === Some(Tree.Leaf("C")))
  }

  test("left goes to the previous sibling") {
    val tree = Tree.Node("A", List(Tree.Leaf("B"), Tree.Leaf("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.right).flatMap(_.left).map(_.get) === Some(Tree.Leaf("B")))
  }

