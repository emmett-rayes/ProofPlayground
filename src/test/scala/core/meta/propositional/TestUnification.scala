package proofPlayground
package core.meta.propositional

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.Pattern
import core.meta.propositional.PatternUtil.asPattern
import core.meta.propositional.Unification

import org.scalatest.funsuite.AnyFunSuite

/** Tests for the [[Unification]] functions. */
class TestUnification extends AnyFunSuite:
  private val formulaGenerator = FormulaGenerationUtil.arbitraryGenerator

  test("identical formulas unify") {
    val formula = formulaGenerator.arbitrary.sample.get
    val pattern = formula.asPattern
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("meta-variables unify any propositional formula") {
    val metavariable = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = metavariable
    val formula = formulaGenerator.arbitrary.sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(metavariable -> formula))
  }

  test("variable pattern unifies same variable formula") {
    val variableSymbol = symbol.Variable[FormulaF.Propositional]()
    val formula = Formula(FormulaF.Variable(variableSymbol))
    val pattern = Pattern.Formula.Concrete(FormulaF.Variable(variableSymbol))
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("variable pattern does not unify different variable formulas") {
    val formula = Formula(variable())
    val pattern = Pattern.Formula.Concrete(variable())
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("true pattern unifies true formula") {
    val pattern = Pattern.Formula.Concrete(tru)
    val formula = Formula(tru)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("true pattern does not unifies anything that is not true formula") {
    val pattern = Pattern.Formula.Concrete(tru)
    val formula = formulaGenerator.arbitrary.retryUntil(f => f != Formula(tru)).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("false pattern unifies false formula") {
    val pattern = Pattern.Formula.Concrete(fls)
    val formula = Formula(fls)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("false pattern does not unifies anything that is not false formula") {
    val pattern = Pattern.Formula.Concrete(tru)
    val formula = formulaGenerator.arbitrary.retryUntil(f => f != Formula(fls)).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("negation patterns unify negation formulas") {
    val subPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = Pattern.Formula.Concrete[FormulaF](~subPattern)
    val subFormula = formulaGenerator.arbitrary.sample.get
    val formula = Formula(~subFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(subPattern -> subFormula))
  }

  test("negation patterns do not unify non-negation formulas") {
    val subPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = Pattern.Formula.Concrete[FormulaF](~subPattern)
    val formula = formulaGenerator.arbitrary.retryUntil(f =>
      f.formula match
        case FormulaF.Negation(_) => false
        case _ => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("conjunction patterns unify conjunction formulas") {
    val leftPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val rightPattern = Pattern.Formula.Meta[FormulaF]("psi")
    val pattern = Pattern.Formula.Concrete[FormulaF](leftPattern /\ rightPattern)
    val leftFormula = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula = Formula(leftFormula /\ rightFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern -> leftFormula, rightPattern -> rightFormula))
  }

  test("conjunction patterns do not unify non-conjunction formulas") {
    val leftPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val rightPattern = Pattern.Formula.Meta[FormulaF]("psi")
    val pattern = Pattern.Formula.Concrete[FormulaF](leftPattern /\ rightPattern)
    val formula = formulaGenerator.arbitrary.retryUntil(f =>
      f.formula match
        case FormulaF.Conjunction(_) => false
        case _ => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("conjunction unification fails when conflicting metavariables occur") {
    val metavariable = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = Pattern.Formula.Concrete[FormulaF](metavariable /\ metavariable)
    val leftFormula = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula = Formula(leftFormula /\ rightFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("disjunction patterns unify disjunction formulas") {
    val leftPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val rightPattern = Pattern.Formula.Meta[FormulaF]("psi")
    val pattern = Pattern.Formula.Concrete[FormulaF](leftPattern \/ rightPattern)
    val leftFormula = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula = Formula(leftFormula \/ rightFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern -> leftFormula, rightPattern -> rightFormula))
  }

  test("disjunction patterns do not unify non-disjunction formulas") {
    val leftPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val rightPattern = Pattern.Formula.Meta[FormulaF]("psi")
    val pattern = Pattern.Formula.Concrete[FormulaF](leftPattern \/ rightPattern)
    val formula = formulaGenerator.arbitrary.retryUntil(f =>
      f.formula match
        case FormulaF.Disjunction(_) => false
        case _ => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("disjunction unification fails when conflicting metavariables occur") {
    val metavariable = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = Pattern.Formula.Concrete[FormulaF](metavariable \/ metavariable)
    val leftFormula = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula = Formula(leftFormula \/ rightFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("implication patterns unify implication formulas") {
    val leftPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val rightPattern = Pattern.Formula.Meta[FormulaF]("psi")
    val pattern = Pattern.Formula.Concrete[FormulaF](leftPattern --> rightPattern)
    val leftFormula = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula = Formula(leftFormula --> rightFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern -> leftFormula, rightPattern -> rightFormula))
  }

  test("implication patterns do not unify non-implication formulas") {
    val leftPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val rightPattern = Pattern.Formula.Meta[FormulaF]("psi")
    val pattern = Pattern.Formula.Concrete[FormulaF](leftPattern --> rightPattern)
    val formula = formulaGenerator.arbitrary.retryUntil(f =>
      f.formula match
        case FormulaF.Implication(_) => false
        case _ => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("implication unification fails when conflicting metavariables occur") {
    val metavariable = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = Pattern.Formula.Concrete[FormulaF](metavariable --> metavariable)
    val leftFormula = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula = Formula(leftFormula --> rightFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }
