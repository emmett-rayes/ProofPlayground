package proofPlayground
package frontend.notation.logic

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import frontend.notation.asTokens

import org.scalatest.funsuite.AnyFunSuite

/** Tests for [[FormulaParser]] functions. */
class TestFormulaParser extends AnyFunSuite:
  import FormulaParser.*

  test("variable parsing recognizes simple propositional variables") {
    val parser = FormulaF.Variable.parser[Formula]
    val input  = "A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A"))
  }

  test("variable parsing recognizes propositional variables with digits") {
    val parser = FormulaF.Variable.parser[Formula]
    val input  = "A2".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A2"))
  }

  test("variable parsing rejects propositional variables with small letters") {
    val parser = FormulaF.Variable.parser[Formula]
    val input  = "a".asTokens

    val result = parser.parse(input)
    assert(result.isFailure)
  }

  test("variable parsing recognizes multiple letters as multiple propositional variables") {
    val parser = FormulaF.Variable.parser[Formula]
    val input  = "AB".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.nonEmpty)
    assert(result.get.parsed == variable("A"))

    val result2 = parser.parse(result.get.remaining)
    assert(result2.isSuccess)
    assert(result2.get.remaining.isEmpty)
    assert(result2.get.parsed == variable("B"))
  }

  test("true parsing recognizes 'True'") {
    val parser = FormulaF.True.parser[Formula]
    val input  = "True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == tru)
  }

  test("true parsing recognizes '⊤'") {
    val parser = FormulaF.True.parser[Formula]
    val input  = "⊤".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == tru)
  }

  test("false parsing recognizes 'False'") {
    val parser = FormulaF.False.parser[Formula]
    val input  = "False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == fls)
  }

  test("false parsing recognizes '⊥'") {
    val parser = FormulaF.False.parser[Formula]
    val input  = "⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == fls)
  }

  test("negation parsing recognizes '~A'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Negation.parser(subparser)
    val input     = raw"~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == ~variable("A"))
  }

  test("negation parsing recognizes '¬A'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Negation.parser(subparser)
    val input     = raw"¬A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == ~variable("A"))
  }

  test(raw"conjunction parsing recognizes 'A /\ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Conjunction.parser(subparser)
    val input     = raw"A /\ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A") /\ variable("B"))
  }

  test(raw"conjunction parsing recognizes 'A ∧ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Conjunction.parser(subparser)
    val input     = raw"A ∧ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A") /\ variable("B"))
  }

  test(raw"disjunction parsing recognizes 'A \/ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Disjunction.parser(subparser)
    val input     = raw"A \/ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A") \/ variable("B"))
  }

  test(raw"disjunction parsing recognizes 'A ∨ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Disjunction.parser(subparser)
    val input     = raw"A ∨ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A") \/ variable("B"))
  }

  test(raw"implication parsing recognizes 'A --> B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Implication.parser(subparser)
    val input     = raw"A --> B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A") --> variable("B"))
  }

  test(raw"implication parsing recognizes 'A → B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Implication.parser(subparser)
    val input     = raw"A → B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == variable("A") --> variable("B"))
  }

  test("formula parsing recognizes formulas with parentheses") {
    val parser = Formula.parser
    val input = raw"(A /\ B)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == Formula(Formula(variable("A")) /\ Formula(variable("B"))))
  }

  test("formula parsing recognizes nested formulas") {
    val parser = Formula.parser
    val input = raw"((A \/ B) --> (C /\ D))".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed == Formula(
      Formula(Formula(variable[Formula]("A")) \/ Formula(variable[Formula]("B"))) -->
        Formula(Formula(variable[Formula]("C")) /\ Formula(variable[Formula]("D")))
    ))
  }
