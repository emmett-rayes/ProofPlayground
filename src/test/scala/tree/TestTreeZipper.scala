package proofPlayground
package tree

import org.scalatest.funsuite.AnyFunSuite

class TestTreeZipper extends AnyFunSuite:

  test("down then up restores the original tree") {
    val tree   = Tree("A", List(Tree("B"), Tree("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.up) === Some(zipper))
  }

  test("down goes to left-most child") {
    val tree   = Tree("A", List(Tree("B"), Tree("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.map(_.get) === Some(Tree("B")))
  }

  test("right goes to the next sibling") {
    val tree   = Tree("A", List(Tree("B"), Tree("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.right).map(_.get) === Some(Tree("C")))
  }

  test("left goes to the previous sibling") {
    val tree   = Tree("A", List(Tree("B"), Tree("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.right).flatMap(_.left).map(_.get) === Some(Tree("B")))
  }

  test("up from root returns None") {
    val tree   = Tree("A", List(Tree("B"), Tree("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.up === None)
  }

  test("down from leaf returns None") {
    val tree   = Tree("A")
    val zipper = TreeZipper(tree)
    assert(zipper.down === None)
  }

  test("left from left-most child returns None") {
    val tree   = Tree("A", List(Tree("B"), Tree("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.left) === None)
  }

  test("right from right-most child returns None") {
    val tree   = Tree("A", List(Tree("B"), Tree("C")))
    val zipper = TreeZipper(tree)
    assert(zipper.down.flatMap(_.right).flatMap(_.right) === None)
  }
