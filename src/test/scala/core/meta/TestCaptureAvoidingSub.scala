package proofPlayground
package core.meta

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.Formula
import core.meta.CaptureAvoidingSub.given
import core.meta.FreeVars.given

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

/** Tests for [[CaptureAvoidingSub]] trait and implementation. */
class TestCaptureAvoidingSub extends AnyFunSuite:

  // Test simple substitution without quantifiers
  test("substitution in a simple variable replaces the variable") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val result = x.substituteWithoutCapturing(x, y)

    assert(result === y)
  }

  test("substitution in a different variable leaves it unchanged") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val result = y.substituteWithoutCapturing(x, z)

    assert(result === y)
  }

  test("substitution in true constant leaves it unchanged") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val result = (tru: Formula).substituteWithoutCapturing(x, y)

    assert(result === tru)
  }

  test("substitution in false constant leaves it unchanged") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val result = (fls: Formula).substituteWithoutCapturing(x, y)

    assert(result === fls)
  }

  test("substitution propagates through negation") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val formula = ~x
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === ~y)
  }

  test("substitution propagates through conjunction") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val formula = x /\ z
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === y /\ z)
  }

  test("substitution propagates through disjunction") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val formula = x \/ z
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === y \/ z)
  }

  test("substitution propagates through implication") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val formula = x --> z
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === y --> z)
  }

  test("substitution in both sides of conjunction") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val formula = x /\ x
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === y /\ y)
  }

  // Test universal quantification
  test("substitution stops at universal quantifier binding the same variable") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val a = variable[Formula]("A")
    val formula = forall[Formula](x, a)
    val result = formula.substituteWithoutCapturing(x, y)

    // x is bound by forall, so substitution should not occur inside
    assert(result === formula)
  }

  test("substitution continues through universal quantifier with different variable") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val formula = forall[Formula](z, x)
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === forall[Formula](z, y))
  }

  test("substitution in universal quantifier body without capture") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val a = variable[Formula]("A")
    val formula = forall[Formula](z, x /\ a)
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === forall[Formula](z, y /\ a))
  }

  // Test existential quantification
  test("substitution stops at existential quantifier binding the same variable") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val a = variable[Formula]("A")
    val formula = exists[Formula](x, a)
    val result = formula.substituteWithoutCapturing(x, y)

    // x is bound by exists, so substitution should not occur inside
    assert(result === formula)
  }

  test("substitution continues through existential quantifier with different variable") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val formula = exists[Formula](z, x)
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === exists[Formula](z, y))
  }

  test("substitution in existential quantifier body without capture") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val a = variable[Formula]("A")
    val formula = exists[Formula](z, x /\ a)
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === exists[Formula](z, y /\ a))
  }

  // Test nested quantifiers
  test("substitution in nested quantifiers") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val w = variable[Formula]("w")
    val formula = forall[Formula](z, exists[Formula](w, x))
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === forall[Formula](z, exists[Formula](w, y)))
  }

  test("substitution stops at inner quantifier when it binds the variable") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val formula = forall[Formula](z, exists[Formula](x, x))
    val result = formula.substituteWithoutCapturing(x, y)

    // Inner exists binds x, so substitution should stop there
    assert(result === formula)
  }

  // Test complex formulas
  test("substitution in complex formula with multiple occurrences") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val a = variable[Formula]("A")
    val formula = (x /\ a) --> (x \/ ~x)
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === (y /\ a) --> (y \/ ~y))
  }

  test("substitution with quantifier in complex formula") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val a = variable[Formula]("A")
    val formula = x --> forall[Formula](z, a)
    val result = formula.substituteWithoutCapturing(x, y)

    assert(result === y --> forall[Formula](z, a))
  }

  test("free variables are computed correctly before substitution") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val formula = forall[Formula](x, x /\ y)

    // y should be free, x should not
    assert(!formula.freevariables.contains(x))
    assert(formula.freevariables.contains(y))
  }

  test("substitution preserves free variables correctly") {
    val x = variable[Formula]("x")
    val y = variable[Formula]("y")
    val z = variable[Formula]("z")
    val a = variable[Formula]("A")

    // forall z. (x /\ A)
    val formula = forall[Formula](z, x /\ a)
    // substitute x with y: forall z. (y /\ A)
    val result = formula.substituteWithoutCapturing(x, y)

    // z should not be free in either
    assert(!formula.freevariables.contains(z))
    assert(!result.freevariables.contains(z))

    // x should be free in original, y should be free in result
    assert(formula.freevariables.contains(x))
    assert(result.freevariables.contains(y))
  }

