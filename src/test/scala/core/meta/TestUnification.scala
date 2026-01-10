package proofPlayground
package core.meta

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.Pattern.given
import core.meta.PatternF.{concrete, meta}
import core.meta.PatternUtil.asPattern
import core.meta.{Pattern, Unification}

import org.scalatest.funsuite.AnyFunSuite

/** Tests for the [[Unification]] functions. */
class TestUnification extends AnyFunSuite:
  private val formulaGenerator = FormulaGenerationUtil.arbitraryGenerator

  test("identical formulas unify") {
    val formula = formulaGenerator.arbitrary.sample.get
    val pattern = formula.asPattern
    val result  = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("meta-variables unify any propositional formula") {
    val metavariable = meta[FormulaF, Pattern[FormulaF]]("phi")
    val pattern      = metavariable
    val formula      = formulaGenerator.arbitrary.sample.get
    val result       = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(metavariable -> formula))
  }

  test("variable pattern unifies same variable formula") {
    val variableSymbol = symbol.Variable[FormulaF.Propositional]()
    val formula        = Formula(FormulaF.Variable(variableSymbol))
    val pattern        = concrete[FormulaF, Pattern[FormulaF]](FormulaF.Variable(variableSymbol))
    val result         = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("variable pattern does not unify different variable formulas") {
    val pattern = concrete[FormulaF, Pattern[FormulaF]](variable())
    val formula = Formula(variable())
    val result  = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("true pattern unifies true formula") {
    val pattern = concrete[FormulaF, Pattern[FormulaF]](tru)
    val formula = Formula(tru)
    val result  = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("true pattern does not unifies anything that is not true formula") {
    val pattern = concrete[FormulaF, Pattern[FormulaF]](tru)
    val formula = formulaGenerator.arbitrary.retryUntil(f => f != Formula(tru)).sample.get
    val result  = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("false pattern unifies false formula") {
    val pattern = concrete[FormulaF, Pattern[FormulaF]](fls)
    val formula = Formula(fls)
    val result  = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("false pattern does not unifies anything that is not false formula") {
    val pattern = concrete[FormulaF, Pattern[FormulaF]](tru)
    val formula = formulaGenerator.arbitrary.retryUntil(f => f != Formula(fls)).sample.get
    val result  = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("negation patterns unify negation formulas") {
    val subPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val pattern    = concrete[FormulaF, Pattern[FormulaF]](~subPattern)
    val subFormula = Formula(variable[Formula]())
    val formula    = Formula(~subFormula)
    val result     = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(subPattern.unfix -> subFormula))
  }

  test("negation patterns do not unify non-negation formulas") {
    val subPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val pattern    = concrete[FormulaF, Pattern[FormulaF]](~subPattern)
    val formula    = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match
        case FormulaF.Negation(_) => false
        case _                    => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("conjunction patterns unify conjunction formulas") {
    val leftPattern  = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val rightPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("psi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](leftPattern /\ rightPattern)
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula      = Formula(leftFormula /\ rightFormula)
    val result       = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern.unfix -> leftFormula, rightPattern.unfix -> rightFormula))
  }

  test("conjunction patterns do not unify non-conjunction formulas") {
    val leftPattern  = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val rightPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("psi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](leftPattern /\ rightPattern)
    val formula      = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match
        case FormulaF.Conjunction(_) => false
        case _                       => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("conjunction unification fails when conflicting metavariables occur") {
    val metavariable = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](metavariable /\ metavariable)
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula      = Formula(leftFormula /\ rightFormula)
    val result       = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("disjunction patterns unify disjunction formulas") {
    val leftPattern  = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val rightPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("psi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](leftPattern \/ rightPattern)
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula      = Formula(leftFormula \/ rightFormula)
    val result       = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern.unfix -> leftFormula, rightPattern.unfix -> rightFormula))
  }

  test("disjunction patterns do not unify non-disjunction formulas") {
    val leftPattern  = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val rightPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("psi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](leftPattern \/ rightPattern)
    val formula      = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match
        case FormulaF.Disjunction(_) => false
        case _                       => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("disjunction unification fails when conflicting metavariables occur") {
    val metavariable = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](metavariable \/ metavariable)
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula      = Formula(leftFormula \/ rightFormula)
    val result       = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("implication patterns unify implication formulas") {
    val leftPattern  = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val rightPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("psi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](leftPattern --> rightPattern)
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.sample.get
    val formula      = Formula(leftFormula --> rightFormula)
    val result       = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(leftPattern.unfix -> leftFormula, rightPattern.unfix -> rightFormula))
  }

  test("implication patterns do not unify non-implication formulas") {
    val leftPattern  = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val rightPattern = Pattern(meta[FormulaF, Pattern[FormulaF]]("psi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](leftPattern --> rightPattern)
    val formula      = formulaGenerator.arbitrary.retryUntil(f =>
      f.unfix match
        case FormulaF.Implication(_) => false
        case _                       => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("implication unification fails when conflicting metavariables occur") {
    val metavariable = Pattern(meta[FormulaF, Pattern[FormulaF]]("phi"))
    val pattern      = concrete[FormulaF, Pattern[FormulaF]](metavariable --> metavariable)
    val leftFormula  = formulaGenerator.arbitrary.sample.get
    val rightFormula = formulaGenerator.arbitrary.retryUntil(f => f != leftFormula).sample.get
    val formula      = Formula(leftFormula --> rightFormula)
    val result       = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("sequence unification with multiple meta-variables starting with meta-variable") {
    val delta: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Delta")
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val disjunction: Pattern[FormulaF]                    = concrete(Pattern(meta("phi")) \/ Pattern(meta("psi")))
    val negation: Pattern[FormulaF]                       = concrete(~Pattern(meta("phi")))
    val patterns: Seq[Pattern[FormulaF]]                  = Seq(delta, disjunction, gamma, negation)

    val varA: Formula          = Formula(variable())
    val varB: Formula          = Formula(variable())
    val formulas: Seq[Formula] = Seq(Formula(varA \/ varB), Formula(tru), Formula(fls), Formula(~varA))

    val unification = Unification.unify(patterns, formulas)
    assert(unification.isDefined)
    assert(unification.get(delta) === Seq.empty)
    assert(unification.get(gamma) === Seq(Formula(tru), Formula(fls)))
  }

  test("sequence unification with multiple meta-variables ending with meta-variable") {
    val delta: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Delta")
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val disjunction: Pattern[FormulaF]                    = concrete(Pattern(meta("phi")) \/ Pattern(meta("psi")))
    val negation: Pattern[FormulaF]                       = concrete(~Pattern(meta("phi")))
    val patterns: Seq[Pattern[FormulaF]]                  = Seq(delta, disjunction, negation, gamma)

    val varA: Formula          = Formula(variable())
    val varB: Formula          = Formula(variable())
    val formulas: Seq[Formula] = Seq(Formula(varA \/ varB), Formula(~varA), Formula(tru), Formula(fls))

    val unification = Unification.unify(patterns, formulas)
    assert(unification.isDefined)
    assert(unification.get(delta) === Seq.empty)
    assert(unification.get(gamma) === Seq(Formula(tru), Formula(fls)))
  }

  test("sequence unification single formula starting with meta-variable") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val disjunction: Pattern[FormulaF]                    = concrete(Pattern(meta("phi")) \/ Pattern(meta("psi")))
    val patterns: Seq[Pattern[FormulaF]]                  = Seq(gamma, disjunction)

    val varA: Formula          = Formula(variable())
    val varB: Formula          = Formula(variable())
    val formulas: Seq[Formula] = Seq(Formula(tru), Formula(fls), Formula(~varA), Formula(varA \/ varB))

    val unification = Unification.unify(patterns, formulas)
    assert(unification.isDefined)
    assert(unification.get(gamma) === Seq(Formula(tru), Formula(fls), Formula(~varA)))
  }

  test("sequence unification single formula ending with meta-variable") {
    val gamma: PatternF.Meta[FormulaF, Pattern[FormulaF]] = meta("Gamma")
    val disjunction: Pattern[FormulaF]                    = concrete(Pattern(meta("phi")) \/ Pattern(meta("psi")))
    val patterns: Seq[Pattern[FormulaF]]                  = Seq(disjunction, gamma)

    val varA: Formula          = Formula(variable())
    val varB: Formula          = Formula(variable())
    val formulas: Seq[Formula] = Seq(Formula(varA \/ varB), Formula(tru), Formula(fls), Formula(~varA))

    val unification = Unification.unify(patterns, formulas)
    assert(unification.isDefined)
    assert(unification.get(gamma) === Seq(Formula(tru), Formula(fls), Formula(~varA)))
  }
