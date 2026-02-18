package proofPlayground
package frontend.notation

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import parser.asTokens

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

/** Tests for [[FormulaParser]] functions. */
class TestFormulaParser extends AnyFunSuite {
  import FormulaParser.*

  test("variable parsing recognizes simple propositional variables") {
    val parser = Formula.parser
    val input  = "A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A"))
  }

  test("variable parsing recognizes propositional variables with digits") {
    val parser = Formula.parser
    val input  = "A2".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable("A2"))
  }

  test("variable parsing rejects propositional variables with small letters") {
    val parser = Formula.parser
    val input  = "a".asTokens

    val result = parser.parse(input)
    assert(result.isFailure)
  }

  test("variable parsing recognizes multiple letters as multiple propositional variables") {
    val parser = Formula.parser
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
    val parser = Formula.parser
    val input  = "True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === tru)
  }

  test("true parsing recognizes '⊤'") {
    val parser = Formula.parser
    val input  = "⊤".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === tru[Formula])
  }

  test("false parsing recognizes 'False'") {
    val parser = Formula.parser
    val input  = "False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === fls[Formula])
  }

  test("false parsing recognizes '⊥'") {
    val parser = Formula.parser
    val input  = "⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === fls[Formula])
  }

  test("negation parsing recognizes '~A'") {
    val parser = Formula.parser
    val input  = raw"~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~variable[Formula]("A"))
  }

  test("negation parsing recognizes '¬A'") {
    val parser = Formula.parser
    val input  = raw"¬A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~variable[Formula]("A"))
  }

  test(raw"conjunction parsing recognizes 'A /\ B'") {
    val parser = Formula.parser
    val input  = raw"A /\ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") /\ variable[Formula]("B"))
  }

  test(raw"conjunction parsing recognizes 'A ∧ B'") {
    val parser = Formula.parser
    val input  = raw"A ∧ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") /\ variable[Formula]("B"))
  }

  test(raw"disjunction parsing recognizes 'A \/ B'") {
    val parser = Formula.parser
    val input  = raw"A \/ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") \/ variable[Formula]("B"))
  }

  test(raw"disjunction parsing recognizes 'A ∨ B'") {
    val parser = Formula.parser
    val input  = raw"A ∨ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") \/ variable[Formula]("B"))
  }

  test(raw"implication parsing recognizes 'A --> B'") {
    val parser = Formula.parser
    val input  = raw"A --> B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") --> variable[Formula]("B"))
  }

  test(raw"implication parsing recognizes 'A → B'") {
    val parser = Formula.parser
    val input  = raw"A → B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") --> variable[Formula]("B"))
  }

  test("formula parsing recognizes formulas with parentheses") {
    val parser = Formula.parser
    val input  = raw"(A /\ B)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") /\ variable[Formula]("B"))
  }

  test("formula parsing recognizes nested formulas") {
    val parser = Formula.parser
    val input  = raw"A \/ B /\ C".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") \/ (variable[Formula]("B") /\ variable[Formula]("C")))
  }

  test("formula parsing recognizes nested formulas with parentheses") {
    val parser = Formula.parser
    val input  = raw"(A \/ B) /\ C".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (variable[Formula]("A") \/ variable[Formula]("B")) /\ variable[Formula]("C"))
  }

  test("formula parsing recognizes complex nested formulas") {
    val parser = Formula.parser
    val input  = raw"(A \/ B) --> ((~C) /\ (~D))".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed ===
      variable[Formula]("A") \/ variable[Formula]("B")
      --> ((~variable[Formula]("C")) /\ (~variable[Formula]("D"))))
  }

  test("formula parsing recognizes double negation") {
    val parser = Formula.parser
    val input  = raw"~~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~(~variable[Formula]("A")))
  }

  test("formula parsing recognizes triple negation") {
    val parser = Formula.parser
    val input  = raw"~~~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~(~(~variable[Formula]("A"))))
  }

  test("formula parsing recognizes single variable") {
    val parser = Formula.parser
    val input  = "A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A"))
  }

  test("formula parsing recognizes True constant") {
    val parser = Formula.parser
    val input  = "True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (tru: Formula))
  }

  test("formula parsing recognizes False constant") {
    val parser = Formula.parser
    val input  = "False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (fls: Formula))
  }

  test("formula parsing recognizes negation of True") {
    val parser = Formula.parser
    val input  = "~True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~(tru: Formula))
  }

  test("formula parsing recognizes negation of False") {
    val parser = Formula.parser
    val input  = "~False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~(fls: Formula))
  }

  test("formula parsing respects conjunction precedence over disjunction") {
    val parser = Formula.parser
    val input  = raw"A \/ B /\ C".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    // Conjunction binds tighter: A \/ (B /\ C)
    assert(result.get.parsed === variable[Formula]("A") \/ (variable[Formula]("B") /\ variable[Formula]("C")))
  }

  test("formula parsing respects implication as lowest precedence") {
    val parser = Formula.parser
    val input  = raw"A \/ B --> C /\ D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    // Implication binds loosest: (A \/ B) --> (C /\ D)
    assert(result.get.parsed === (variable[Formula]("A") \/ variable[Formula]("B")) --> ((variable(
      "C"
    ): Formula) /\ variable[Formula]("D")))
  }

  test("formula parsing with deeply nested parentheses") {
    val parser = Formula.parser
    val input  = raw"(((A)))".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A"))
  }

  test("formula parsing with mixed Unicode and ASCII operators") {
    val parser = Formula.parser
    val input  = raw"A ∧ B \/ C → D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ((variable[Formula]("A") /\ variable[Formula]("B")) \/ (variable(
      "C"
    ): Formula)) --> variable[Formula]("D"))
  }

  test("formula parsing recognizes ⊤ constant") {
    val parser = Formula.parser
    val input  = "⊤".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (tru: Formula))
  }

  test("formula parsing recognizes ⊥ constant") {
    val parser = Formula.parser
    val input  = "⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (fls: Formula))
  }

  test("formula parsing recognizes negation with ¬ symbol") {
    val parser = Formula.parser
    val input  = "¬A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~variable[Formula]("A"))
  }

  test("formula parsing with ⊤ and ⊥ in expression") {
    val parser = Formula.parser
    val input  = raw"⊤ /\ ⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (tru: Formula) /\ (fls: Formula))
  }

  test("formula parsing recognizes implication with variable on both sides") {
    val parser = Formula.parser
    val input  = raw"A --> B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") --> variable[Formula]("B"))
  }

  test("formula parsing recognizes complex expression with all operators") {
    val parser = Formula.parser
    val input  = raw"(A /\ B) \/ (~C --> D)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (variable[Formula]("A") /\ (variable(
      "B"
    ): Formula)) \/ ((~variable[Formula]("C")) --> variable[Formula]("D")))
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
    assert(result.get.parsed === variable[Formula]("A123"))
  }

  test("formula parsing recognizes multiple different variables") {
    val parser = Formula.parser
    val input  = raw"X /\ Y /\ Z".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (variable[Formula]("X") /\ variable[Formula]("Y")) /\ variable[Formula]("Z"))
  }

  test("negation parsing with parenthesized subformula") {
    val parser = Formula.parser
    val input  = raw"~(A /\ B)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === ~(variable[Formula]("A") /\ variable[Formula]("B")))
  }

  test("formula parsing with alternating conjunctions and disjunctions") {
    val parser = Formula.parser
    val input  = raw"A /\ B \/ C /\ D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    // Due to precedence: (A /\ B) \/ (C /\ D)
    assert(result.get.parsed ===
      (variable[Formula]("A") /\ variable[Formula]("B")) \/ (variable[Formula]("C") /\ variable[Formula]("D")))
  }

  test("chained conjunctions are left associative") {
    val parser = Formula.parser
    val input  = raw"A /\ B /\ C".asTokens
    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (variable[Formula]("A") /\ variable[Formula]("B")) /\ variable[Formula]("C"))
  }

  test("chained disjunctions are left associative") {
    val parser = Formula.parser
    val input  = raw"A \/ B \/ C".asTokens
    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === (variable[Formula]("A") \/ variable[Formula]("B")) \/ variable[Formula]("C"))
  }

  test("chained implications are right associative") {
    val parser = Formula.parser
    val input  = "A --> B --> C".asTokens
    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === variable[Formula]("A") --> (variable[Formula]("B") --> variable[Formula]("C")))
  }

  test("universal quantification parsing recognizes 'forall X. A'") {
    val parser = Formula.parser
    val input  = "forall X. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), variable[Formula]("A")))
  }

  test("universal quantification parsing recognizes '∀ X. A'") {
    val parser = Formula.parser
    val input  = "∀ X. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), variable[Formula]("A")))
  }

  test("existential quantification parsing recognizes 'exists X. A'") {
    val parser = Formula.parser
    val input  = "exists X. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), variable[Formula]("A")))
  }

  test("existential quantification parsing recognizes '∃ X. A'") {
    val parser = Formula.parser
    val input  = "∃ X. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), variable[Formula]("A")))
  }

  test("universal quantification with conjunction in body") {
    val parser = Formula.parser
    val input  = raw"forall X. A /\ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), variable[Formula]("A") /\ variable[Formula]("B")))
  }

  test("existential quantification with disjunction in body") {
    val parser = Formula.parser
    val input  = raw"exists X. A \/ B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), variable[Formula]("A") \/ variable[Formula]("B")))
  }

  test("universal quantification with negation in body") {
    val parser = Formula.parser
    val input  = raw"forall X. ~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), ~variable[Formula]("A")))
  }

  test("existential quantification with implication in body") {
    val parser = Formula.parser
    val input  = raw"exists X. A --> B".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), variable[Formula]("A") --> variable[Formula]("B")))
  }

  test("nested universal quantifications") {
    val parser = Formula.parser
    val input  = "forall X. forall Y. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), forall[Formula](variable[Formula]("Y"), variable[Formula]("A"))))
  }

  test("nested existential quantifications") {
    val parser = Formula.parser
    val input  = "exists X. exists Y. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), exists[Formula](variable[Formula]("Y"), variable[Formula]("A"))))
  }

  test("mixed nested quantifications - universal then existential") {
    val parser = Formula.parser
    val input  = "forall X. exists Y. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), exists[Formula](variable[Formula]("Y"), variable[Formula]("A"))))
  }

  test("mixed nested quantifications - existential then universal") {
    val parser = Formula.parser
    val input  = "exists X. forall Y. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), forall[Formula](variable[Formula]("Y"), variable[Formula]("A"))))
  }

  test("universal quantification with parenthesized body") {
    val parser = Formula.parser
    val input  = raw"forall X. (A /\ B)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), variable[Formula]("A") /\ variable[Formula]("B")))
  }

  test("existential quantification with parenthesized body") {
    val parser = Formula.parser
    val input  = raw"exists X. (A \/ B)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), variable[Formula]("A") \/ variable[Formula]("B")))
  }

  test("universal quantification with complex nested formula") {
    val parser = Formula.parser
    val input  = raw"forall X. (A /\ B) \/ (~C --> D)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"),
      (variable[Formula]("A") /\ variable[Formula]("B")) \/ ((~variable[Formula]("C")) --> variable[Formula]("D"))))
  }

  test("existential quantification with complex nested formula") {
    val parser = Formula.parser
    val input  = raw"exists X. (A --> B) /\ (C \/ ~D)".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"),
      (variable[Formula]("A") --> variable[Formula]("B")) /\ (variable[Formula]("C") \/ ~variable[Formula]("D"))))
  }

  test("triple nested quantifications") {
    val parser = Formula.parser
    val input  = "forall X. exists Y. forall Z. A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"),
      exists[Formula](variable[Formula]("Y"),
        forall[Formula](variable[Formula]("Z"), variable[Formula]("A")))))
  }

  test("universal quantification over True constant") {
    val parser = Formula.parser
    val input  = "forall X. True".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), tru: Formula))
  }

  test("existential quantification over False constant") {
    val parser = Formula.parser
    val input  = "exists X. False".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), fls: Formula))
  }

  test("universal quantification with ⊤") {
    val parser = Formula.parser
    val input  = "forall X. ⊤".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), tru: Formula))
  }

  test("existential quantification with ⊥") {
    val parser = Formula.parser
    val input  = "exists X. ⊥".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), fls: Formula))
  }

  test("universal quantification with numbered variables") {
    val parser = Formula.parser
    val input  = "forall X1. A2".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X1"), variable[Formula]("A2")))
  }

  test("existential quantification with numbered variables") {
    val parser = Formula.parser
    val input  = "exists Y3. B5".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("Y3"), variable[Formula]("B5")))
  }

  test("universal quantification with mixed operators using Unicode symbols") {
    val parser = Formula.parser
    val input  = raw"∀ X. A ∧ B ∨ ¬C → D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"),
      ((variable[Formula]("A") /\ variable[Formula]("B")) \/ (~variable[Formula]("C"))) --> variable[Formula]("D")))
  }

  test("existential quantification with mixed operators using Unicode symbols") {
    val parser = Formula.parser
    val input  = raw"∃ X. A ∨ B ∧ ¬C → D".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"),
      (variable[Formula]("A") \/ (variable[Formula]("B") /\ ~variable[Formula]("C"))) --> variable[Formula]("D")))
  }

  test("formula parsing fails on quantification without body") {
    val parser = Formula.parser
    val input  = "forall X.".asTokens

    val result = parser.parse(input)
    assert(result.isFailure)
  }

  test("formula parsing fails on existential without body") {
    val parser = Formula.parser
    val input  = "exists X.".asTokens

    val result = parser.parse(input)
    assert(result.isFailure)
  }

  test("universal quantification with double negation in body") {
    val parser = Formula.parser
    val input  = "forall X. ~~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === forall[Formula](variable[Formula]("X"), ~(~variable[Formula]("A"))))
  }

  test("existential quantification with double negation in body") {
    val parser = Formula.parser
    val input  = "exists X. ~~A".asTokens

    val result = parser.parse(input)
    assert(result.isSuccess)
    assert(result.get.remaining.isEmpty)
    assert(result.get.parsed === exists[Formula](variable[Formula]("X"), ~(~variable[Formula]("A"))))
  }
}

