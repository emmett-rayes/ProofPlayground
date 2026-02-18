package proofPlayground
package core.logic.symbol

import org.scalatest.funsuite.AnyFunSuite

/** Tests for the [[Variable]] class and its companion object. */
final class TestVariable extends AnyFunSuite {

  test("two different variables are not equal") {
    sealed trait DummyKind

    val v1 = Variable[DummyKind]("V1")
    val v2 = Variable[DummyKind]("V2")
    assert(v1 !== v2)
  }

  test("two different variables have different ids") {
    sealed trait DummyKind

    val v1 = Variable[DummyKind]("V1")
    val v2 = Variable[DummyKind]("V2")
    assert(v1.id !== v2.id)
  }

  test("variables of different kinds are not equal") {
    sealed trait DummyKind1
    sealed trait DummyKind2

    val v1k1 = Variable[DummyKind1]("V1")
    val v1k2 = Variable[DummyKind2]("V1")
    assert(v1k1 !== v1k2)
  }

  test("variable kinds are invariant") {
    sealed trait DummyKind1
    sealed trait DummyKind2 extends DummyKind1

    val v1k1 = Variable[DummyKind1]("V1")
    val v1k2 = Variable[DummyKind2]("V1")
    assert(v1k1 !== v1k2)
  }
}
