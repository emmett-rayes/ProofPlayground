package proofPlayground
package tree

import org.scalatest.funsuite.AnyFunSuite

import Zipper.root
import TreeZipper.given

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

  test("root returns root") {
    val tree   = Tree("A", List(Tree("AA", List(Tree("AB"), Tree("AC"))), Tree("AA", List(Tree("BA"), Tree("CA")))))
    val zipper = TreeZipper(tree)
    val leaf   = zipper.down.get.down.get.right.get
    assert(leaf.root === zipper)
  }
