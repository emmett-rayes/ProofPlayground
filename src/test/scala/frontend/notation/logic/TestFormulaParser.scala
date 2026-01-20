package proofPlayground
package frontend.notation.logic

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import frontend.notation.parser.asTokens

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
    assert(result.get.parsed === variable("A"))
  }

  test("variable parsing recognizes propositional variables with digits") {
    val parser = FormulaF.Variable.parser[Formula]
    val input  = "A2".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A2"))
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
    assert(result.get.parsed === variable("A"))

    val result2 = parser.parse(result.get.remaining)
    assert(result2.isSuccess)
    assert(result2.get.remaining.isEmpty)
    assert(result2.get.parsed === variable("B"))
  }

  test("true parsing recognizes 'True'") {
    val parser = FormulaF.True.parser[Formula]
    val input  = "True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === tru)
  }

  test("true parsing recognizes '⊤'") {
    val parser = FormulaF.True.parser[Formula]
    val input  = "⊤".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === tru)
  }

  test("false parsing recognizes 'False'") {
    val parser = FormulaF.False.parser[Formula]
    val input  = "False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === fls)
  }

  test("false parsing recognizes '⊥'") {
    val parser = FormulaF.False.parser[Formula]
    val input  = "⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === fls)
  }

  test("negation parsing recognizes '~A'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Negation.parser(subparser)
    val input     = raw"~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~variable("A"))
  }

  test("negation parsing recognizes '¬A'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Negation.parser(subparser)
    val input     = raw"¬A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~variable("A"))
  }

  test(raw"conjunction parsing recognizes 'A /\ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Conjunction.parser(subparser)
    val input     = raw"A /\ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A") /\ variable("B"))
  }

  test(raw"conjunction parsing recognizes 'A ∧ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Conjunction.parser(subparser)
    val input     = raw"A ∧ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A") /\ variable("B"))
  }

  test(raw"disjunction parsing recognizes 'A \/ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Disjunction.parser(subparser)
    val input     = raw"A \/ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A") \/ variable("B"))
  }

  test(raw"disjunction parsing recognizes 'A ∨ B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Disjunction.parser(subparser)
    val input     = raw"A ∨ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A") \/ variable("B"))
  }

  test(raw"implication parsing recognizes 'A --> B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Implication.parser(subparser)
    val input     = raw"A --> B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A") --> variable("B"))
  }

  test(raw"implication parsing recognizes 'A → B'") {
    val subparser = FormulaF.Variable.parser[Formula]
    val parser    = FormulaF.Implication.parser(subparser)
    val input     = raw"A → B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A") --> variable("B"))
  }

  test("formula parsing recognizes formulas with parentheses") {
    val parser = Formula.parser
    val input  = raw"(A /\ B)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(Formula(variable("A")) /\ Formula(variable("B"))))
  }

  test("formula parsing recognizes nested formulas") {
    val parser = Formula.parser
    val input  = raw"A \/ B /\ C".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(
      Formula(variable[Formula]("A")) \/ Formula(Formula(variable[Formula]("B")) /\ Formula(variable[Formula]("C")))
    ))
  }

  test("formula parsing recognizes nested formulas with parentheses") {
    val parser = Formula.parser
    val input  = raw"(A \/ B) /\ C".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(
      Formula(Formula(variable[Formula]("A")) \/ Formula(variable[Formula]("B"))) /\ Formula(variable[Formula]("C"))
    ))
  }

  test("formula parsing recognizes complex nested formulas") {
    val parser = Formula.parser
    val input = raw"(A \/ B) --> ((~C) /\ (~D))".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(
      Formula(Formula(variable[Formula]("A")) \/ Formula(variable[Formula]("B"))) -->
        Formula(Formula(~Formula(variable[Formula]("C"))) /\ Formula(~Formula(variable[Formula]("D"))))
    ))
  }

  test("formula parsing recognizes double negation") {
    val parser = Formula.parser
    val input  = raw"~~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(~Formula(~Formula(variable[Formula]("A")))))
  }

  test("formula parsing recognizes triple negation") {
    val parser = Formula.parser
    val input  = raw"~~~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(~Formula(~Formula(~Formula(variable[Formula]("A"))))))
  }

  test("formula parsing recognizes single variable") {
    val parser = Formula.parser
    val input  = "A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(variable[Formula]("A")))
  }

  test("formula parsing recognizes True constant") {
    val parser = Formula.parser
    val input  = "True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(tru[Formula]))
  }

  test("formula parsing recognizes False constant") {
    val parser = Formula.parser
    val input  = "False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(fls[Formula]))
  }

  test("formula parsing recognizes negation of True") {
    val parser = Formula.parser
    val input  = "~True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(~Formula(tru[Formula])))
  }

  test("formula parsing recognizes negation of False") {
    val parser = Formula.parser
    val input  = "~False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(~Formula(fls[Formula])))
  }

  test("formula parsing respects conjunction precedence over disjunction") {
    val parser = Formula.parser
    val input  = raw"A \/ B /\ C".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    // Conjunction binds tighter: A \/ (B /\ C)
    assert(result.get.parsed === Formula(
      Formula(variable[Formula]("A")) \/ Formula(Formula(variable[Formula]("B")) /\ Formula(variable[Formula]("C")))
    ))
  }

  test("formula parsing respects implication as lowest precedence") {
    val parser = Formula.parser
    val input  = raw"A \/ B --> C /\ D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    // Implication binds loosest: (A \/ B) --> (C /\ D)
    assert(result.get.parsed === Formula(
      Formula(Formula(variable[Formula]("A")) \/ Formula(variable[Formula]("B"))) -->
        Formula(Formula(variable[Formula]("C")) /\ Formula(variable[Formula]("D")))
    ))
  }

  test("formula parsing with deeply nested parentheses") {
    val parser = Formula.parser
    val input  = raw"(((A)))".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(variable[Formula]("A")))
  }

  test("formula parsing with mixed Unicode and ASCII operators") {
    val parser = Formula.parser
    val input  = raw"A ∧ B \/ C → D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(
      Formula(Formula(Formula(variable[Formula]("A")) /\ Formula(variable[Formula]("B"))) \/ Formula(variable[Formula]("C"))) -->
        Formula(variable[Formula]("D"))
    ))
  }

  test("formula parsing recognizes ⊤ constant") {
    val parser = Formula.parser
    val input  = "⊤".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(tru[Formula]))
  }

  test("formula parsing recognizes ⊥ constant") {
    val parser = Formula.parser
    val input  = "⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(fls[Formula]))
  }

  test("formula parsing recognizes negation with ¬ symbol") {
    val parser = Formula.parser
    val input  = "¬A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(~Formula(variable[Formula]("A"))))
  }

  test("formula parsing with ⊤ and ⊥ in expression") {
    val parser = Formula.parser
    val input  = raw"⊤ /\ ⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(Formula(tru[Formula]) /\ Formula(fls[Formula])))
  }

  test("formula parsing recognizes implication with variable on both sides") {
    val parser = Formula.parser
    val input  = raw"A --> B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(Formula(variable[Formula]("A")) --> Formula(variable[Formula]("B"))))
  }

  test("formula parsing recognizes complex expression with all operators") {
    val parser = Formula.parser
    val input  = raw"(A /\ B) \/ (~C --> D)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(
      Formula(Formula(variable[Formula]("A")) /\ Formula(variable[Formula]("B"))) \/
        Formula(
          Formula(~Formula(variable[Formula]("C"))) -->
          Formula(variable[Formula]("D"))
        )
    ))
  }

  test("variable parsing rejects empty input") {
    val parser = FormulaF.Variable.parser[Formula]
    val input  = "".asTokens

    val result = parser.parse(input)
    assert(result.isFailure)
  }

  test("formula parsing fails on incomplete conjunction") {
    val parser = Formula.parser
    val input  = raw"A /\ ".asTokens

    val result = parser.parse(input)
    // Either fails or has remaining input
    assert(result.isFailure || result.get.remaining.nonEmpty)
  }

  test("formula parsing fails on incomplete disjunction") {
    val parser = Formula.parser
    val input  = raw"A \/ ".asTokens

    val result = parser.parse(input)
    // Either fails or has remaining input
    assert(result.isFailure || result.get.remaining.nonEmpty)
  }

  test("formula parsing fails on incomplete implication") {
    val parser = Formula.parser
    val input  = raw"A --> ".asTokens

    val result = parser.parse(input)
    // Either fails or has remaining input
    assert(result.isFailure || result.get.remaining.nonEmpty)
  }

  test("formula parsing fails on incomplete negation") {
    val parser = Formula.parser
    val input  = "~".asTokens

    val result = parser.parse(input)
    assert(result.isFailure)
  }

  test("formula parsing fails on unmatched opening parenthesis") {
    val parser = Formula.parser
    val input  = "(A".asTokens

    val result = parser.parse(input)
    // Either fails or has remaining input
    assert(result.isFailure || result.get.remaining.nonEmpty)
  }

  test("formula parsing handles variable with multiple digits") {
    val parser = Formula.parser
    val input  = "A123".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(variable[Formula]("A123")))
  }

  test("formula parsing recognizes multiple different variables") {
    val parser = Formula.parser
    val input  = raw"X /\ Y /\ Z".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(
      Formula(Formula(variable[Formula]("X")) /\ Formula(variable[Formula]("Y"))) /\ Formula(variable[Formula]("Z"))
    ))
  }

  test("negation parsing with parenthesized subformula") {
    val parser = Formula.parser
    val input  = raw"~(A /\ B)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === Formula(~Formula(Formula(variable[Formula]("A")) /\ Formula(variable[Formula]("B")))))
  }

  test("formula parsing with alternating conjunctions and disjunctions") {
    val parser = Formula.parser
    val input  = raw"A /\ B \/ C /\ D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    // Due to precedence: (A /\ B) \/ (C /\ D)
    assert(result.get.parsed === Formula(
      Formula(Formula(variable[Formula]("A")) /\ Formula(variable[Formula]("B"))) \/
        Formula(Formula(variable[Formula]("C")) /\ Formula(variable[Formula]("D")))
    ))
  }

