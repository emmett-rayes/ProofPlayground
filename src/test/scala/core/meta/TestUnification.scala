package proofPlayground
package core.meta

import core.fix
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.Pattern.given
import core.meta.PatternF.{concrete, meta}
import core.meta.Unify.given
import core.meta.{Pattern, MapUnification}

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

/** Tests for the [[MapUnification]] functions. */
class TestUnification extends AnyFunSuite {
  private val formulaGenerator = FormulaGenerationUtil.arbitraryGenerator

  private given Conversion[FormulaF[Pattern[FormulaF]], Pattern[FormulaF]] = concrete(_).fix

  test("identical formulas unify") {
    val formula = formulaGenerator.arbitrary.sample.get
    val pattern = formula.asPattern
    val result  = pattern.unifier(formula)

    assert(result.isDefined)
  }

  test("meta-variables unify any propositional formula") {
    val metavariable: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("phi")

    val pattern = metavariable: Pattern[FormulaF]
    val formula = formulaGenerator.arbitrary.sample.get
    val result  = pattern.unifier(formula)

    assert(result.isDefined)
    assert(result.get === Map(metavariable -> formula))
  }

  test("variable pattern unifies same variable formula") {
    val variableSymbol = symbol.Variable[FormulaF.Propositional]("A")
    val formula        = variable(variableSymbol): Formula
    val pattern        = variable(variableSymbol): Pattern[FormulaF]
    val result         = pattern.unifier(formula)

    assert(result.isDefined)
  }

  test("variable pattern does not unify different variable formulas") {
    val pattern = variable("A"): Pattern[FormulaF]
    val formula = variable("B"): Formula
    val result  = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("true pattern unifies true formula") {
    val pattern = tru: Pattern[FormulaF]
    val formula = tru: Formula
    val result  = pattern.unifier(formula)

    assert(result.isDefined)
  }

  test("true pattern does not unifies anything that is not true formula") {
    val pattern = tru: Pattern[FormulaF]
    val formula = formulaGenerator.arbitrary.retryUntil(f => f != tru[Formula]).sample.get
    val result  = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("false pattern unifies false formula") {
    val pattern = fls: Pattern[FormulaF]
    val formula = fls: Formula
    val result  = pattern.unifier(formula)

    assert(result.isDefined)
  }

  test("false pattern does not unifies anything that is not false formula") {
    val pattern = tru: Pattern[FormulaF]
    val formula = formulaGenerator.arbitrary.retryUntil(f => f != fls[Formula]).sample.get
    val result  = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("negation patterns unify negation formulas") {
    val subPattern = meta("phi"): Pattern[FormulaF]
    val pattern    = ~subPattern
    val subFormula = variable("A"): Formula
    val formula    = ~subFormula
    val result     = pattern.unifier(formula)

    assert(result.isDefined)
    assert(result.get === Map(subPattern.unfix -> subFormula))
  }

  test("negation patterns do not unify non-negation formulas") {
    val subPattern = meta("phi"): Pattern[FormulaF]
    val pattern    = ~subPattern
    val formula    = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match {
        case FormulaF.Negation(_) => false
        case _                    => true
      }
    ).sample.get
    val result = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("conjunction patterns unify conjunction formulas") {
    val leftPattern  = meta("phi"): Pattern[FormulaF]
    val rightPattern = meta("psi"): Pattern[FormulaF]
    val pattern      = leftPattern /\ rightPattern
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula      = leftFormula /\ rightFormula
    val result       = pattern.unifier(formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern.unfix -> leftFormula, rightPattern.unfix -> rightFormula))
  }

  test("conjunction patterns do not unify non-conjunction formulas") {
    val leftPattern  = meta("phi"): Pattern[FormulaF]
    val rightPattern = meta("psi"): Pattern[FormulaF]
    val pattern      = leftPattern /\ rightPattern
    val formula      = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match {
        case FormulaF.Conjunction(_) => false
        case _                       => true
      }
    ).sample.get
    val result = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("conjunction unification fails when conflicting metavariables occur") {
    val metavariable = meta("phi"): Pattern[FormulaF]
    val pattern      = metavariable /\ metavariable
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula      = leftFormula /\ rightFormula
    val result       = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("disjunction patterns unify disjunction formulas") {
    val leftPattern  = meta("phi"): Pattern[FormulaF]
    val rightPattern = meta("psi"): Pattern[FormulaF]
    val pattern      = leftPattern \/ rightPattern
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula      = leftFormula \/ rightFormula
    val result       = pattern.unifier(formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern.unfix -> leftFormula, rightPattern.unfix -> rightFormula))
  }

  test("disjunction patterns do not unify non-disjunction formulas") {
    val leftPattern  = meta("phi"): Pattern[FormulaF]
    val rightPattern = meta("psi"): Pattern[FormulaF]
    val pattern      = leftPattern \/ rightPattern
    val formula      = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match {
        case FormulaF.Disjunction(_) => false
        case _                       => true
      }
    ).sample.get
    val result = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("disjunction unification fails when conflicting metavariables occur") {
    val metavariable = meta("phi"): Pattern[FormulaF]
    val pattern      = metavariable \/ metavariable
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula      = leftFormula \/ rightFormula
    val result       = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("implication patterns unify implication formulas") {
    val leftPattern  = meta("phi"): Pattern[FormulaF]
    val rightPattern = meta("psi"): Pattern[FormulaF]
    val pattern      = leftPattern --> rightPattern
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula      = leftFormula --> rightFormula
    val result       = pattern.unifier(formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern.unfix -> leftFormula, rightPattern.unfix -> rightFormula))
  }

  test("implication patterns do not unify non-implication formulas") {
    val leftPattern  = meta("phi"): Pattern[FormulaF]
    val rightPattern = meta("psi"): Pattern[FormulaF]
    val pattern      = leftPattern --> rightPattern
    val formula      = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match {
        case FormulaF.Implication(_) => false
        case _                       => true
      }
    ).sample.get
    val result = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("implication unification fails when conflicting metavariables occur") {
    val metavariable = meta("phi"): Pattern[FormulaF]
    val pattern      = metavariable --> metavariable
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula      = leftFormula --> rightFormula
    val result       = pattern.unifier(formula)

    assert(result.isEmpty)
  }

  test("sequence unification with a single meta-variable") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val patterns: Seq[Pattern[FormulaF]]                  = Seq(gamma)

    val varA     = variable("A"): Formula
    val varB     = variable("B"): Formula
    val formulas = Seq[Formula](varA \/ varB, tru, fls, ~varA)

    val unification = patterns.unifier(formulas)
    assert(unification.isDefined)
    assert(unification.get(gamma) === formulas)
  }

  test("sequence unification with two meta-variables") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val delta: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Delta")
    val patterns: Seq[Pattern[FormulaF]]                  = Seq(gamma, delta)

    val varA                   = variable("A"): Formula
    val varB                   = variable("B"): Formula
    val formulas: Seq[Formula] = Seq(varA \/ varB, tru, fls, ~varA)

    val unification = patterns.unifier(formulas)
    assert(unification.isDefined)
    assert(unification.get(gamma) === formulas)
    assert(unification.get(delta) === Seq.empty)
  }

  test("sequence unification with multiple meta-variables starting with meta-variable") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val delta: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Delta")
    val disjunction                      = (meta("phi"): Pattern[FormulaF]) \/ (meta("psi"): Pattern[FormulaF])
    val negation                         = ~(meta("phi"): Pattern[FormulaF])
    val patterns: Seq[Pattern[FormulaF]] = Seq(delta, disjunction, gamma, negation)

    val varA                   = variable("A"): Formula
    val varB                   = variable("B"): Formula
    val formulas: Seq[Formula] = Seq(varA \/ varB, tru, fls, ~varA)

    val unification = patterns.unifier(formulas)
    assert(unification.isDefined)
    assert(unification.get(delta) === Seq.empty)
    assert(unification.get(gamma) === Seq[Formula](tru, fls))
  }

  test("sequence unification with multiple meta-variables ending with meta-variable") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val delta: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Delta")
    val disjunction                      = (meta("phi"): Pattern[FormulaF]) \/ (meta("psi"): Pattern[FormulaF])
    val negation                         = ~(meta("phi"): Pattern[FormulaF])
    val patterns: Seq[Pattern[FormulaF]] = Seq(delta, disjunction, negation, gamma)

    val varA                   = variable("A"): Formula
    val varB                   = variable("B"): Formula
    val formulas: Seq[Formula] = Seq(varA \/ varB, ~varA, tru, fls)

    val unification = patterns.unifier(formulas)
    assert(unification.isDefined)
    assert(unification.get(delta) === Seq.empty)
    assert(unification.get(gamma) === Seq[Formula](tru, fls))
  }

  test("sequence unification single formula starting with meta-variable") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val disjunction                      = (meta("phi"): Pattern[FormulaF]) \/ (meta("psi"): Pattern[FormulaF])
    val patterns: Seq[Pattern[FormulaF]] = Seq(gamma, disjunction)

    val varA                   = variable("A"): Formula
    val varB                   = variable("B"): Formula
    val formulas: Seq[Formula] = Seq(tru, fls, ~varA, varA \/ varB)

    val unification = patterns.unifier(formulas)
    assert(unification.isDefined)
    assert(unification.get(gamma) === Seq[Formula](tru, fls, ~varA))
  }

  test("sequence unification single formula ending with meta-variable") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val disjunction                      = (meta("phi"): Pattern[FormulaF]) \/ (meta("psi"): Pattern[FormulaF])
    val patterns: Seq[Pattern[FormulaF]] = Seq(disjunction, gamma)

    val varA                   = variable("A"): Formula
    val varB                   = variable("B"): Formula
    val formulas: Seq[Formula] = Seq(varA \/ varB, tru, fls, ~varA)

    val unification = patterns.unifier(formulas)
    assert(unification.isDefined)
    assert(unification.get(gamma) === Seq[Formula](tru, fls, ~varA))
  }
}
