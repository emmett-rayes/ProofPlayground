package proofPlayground
package core.logic.symbol

import org.scalatest.funsuite.AnyFunSuite

final class TestVariable extends AnyFunSuite:
  sealed trait DummyKind

  test("a variable is equal to its copy") {
    val v1 = Variable[DummyKind]()
    val v2 = v1.copy()
    assert(v1 === v2)
  }

  test("a variable and its copy have the same ids") {
    val v1 = Variable[DummyKind]()
    val v2 = v1.copy()
    assert(v1.id === v2.id)
  }

  test("two different variables are not equal") {
    val v1 = Variable[DummyKind]()
    val v2 = Variable[DummyKind]()
    assert(v1 !== v2)
  }

  test("two different variables have different ids") {
    val v1 = Variable[DummyKind]()
    val v2 = Variable[DummyKind]()
    assert(v1.id !== v2.id)
  }
