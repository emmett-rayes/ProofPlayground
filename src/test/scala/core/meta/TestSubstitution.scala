package proofPlayground
package core.meta

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.Pattern.given
import core.meta.PatternF.{concrete, meta}
import core.meta.{Pattern, MapUnification}

import org.scalatest.funsuite.AnyFunSuite
import core.fix

import scala.language.implicitConversions

/** Tests for [[Substitution]] functions. */
class TestSubstitution extends AnyFunSuite {
  private val formulaGenerator = FormulaGenerationUtil.arbitraryGenerator

  private given Conversion[FormulaF[Pattern[FormulaF]], Pattern[FormulaF]] = concrete(_).fix

  test("meta-variables can be substituted with any formula") {
    val metavariable = meta[FormulaF, Pattern[FormulaF]]("phi")
    val pattern      = metavariable: Pattern[FormulaF]
    val formula      = formulaGenerator.arbitrary.sample.get
    val unification  = Map(metavariable -> formula): MapUnification[Formula]
    val result       = pattern.substitute(unification)

    assert(result.isDefined)
    assert(result.get === formula)
  }

  test("substitution fails for unbound meta-variables") {
    val metavariable = meta[FormulaF, Pattern[FormulaF]]("phi")
    val pattern      = metavariable: Pattern[FormulaF]
    val result       = pattern.substitute(Map.empty)

    assert(result.isEmpty)
  }

  test("concrete patterns are not affected by substitution") {
    val formula = formulaGenerator.arbitrary.sample.get
    val pattern = formula.asPattern
    val result  = pattern.substitute(Map.empty)

    assert(result.isDefined)
    assert(result.get === formula)
  }

  test("variable pattern is not affected by substitution") {
    val variableSymbol = symbol.Variable[FormulaF.Propositional]("A")
    val pattern        = variable[Pattern[FormulaF]](variableSymbol)
    val result         = pattern.substitute(Map.empty)

    assert(result.isDefined)
    assert(result.get === Formula(FormulaF.Variable(variableSymbol)))
  }

  test("true pattern is not affected by substitution") {
    val pattern = tru[Pattern[FormulaF]]
    val result  = pattern.substitute(Map.empty)

    assert(result.isDefined)
    assert(result.get === tru[Formula])
  }

  test("false pattern is not affected by substitution") {
    val pattern = fls[Pattern[FormulaF]]
    val result  = pattern.substitute(Map.empty)

    assert(result.isDefined)
    assert(result.get === fls[Formula])
  }

  test("conjunction propagates substitutions") {
    val phi         = meta[FormulaF, Pattern[FormulaF]]("phi")
    val psi         = meta[FormulaF, Pattern[FormulaF]]("psi")
    val pattern     = (phi: Pattern[FormulaF]) /\ (psi: Pattern[FormulaF])
    val formula1    = formulaGenerator.arbitrary.sample.get
    val formula2    = formulaGenerator.arbitrary.sample.get
    val unification = Map(phi -> formula1, psi -> formula2): MapUnification[Formula]
    val result      = pattern.substitute(unification)

    assert(result.isDefined)
    assert(result.get === formula1 /\ formula2)
  }

  test("disjunction propagates substitutions") {
    val phi         = meta[FormulaF, Pattern[FormulaF]]("phi")
    val psi         = meta[FormulaF, Pattern[FormulaF]]("psi")
    val pattern     = (phi: Pattern[FormulaF]) \/ (psi: Pattern[FormulaF])
    val formula1    = formulaGenerator.arbitrary.sample.get
    val formula2    = formulaGenerator.arbitrary.sample.get
    val unification = Map(phi -> formula1, psi -> formula2): MapUnification[Formula]
    val result      = pattern.substitute(unification)

    assert(result.isDefined)
    assert(result.get === formula1 \/ formula2)
  }

  test("implication propagates substitutions") {
    val phi         = meta[FormulaF, Pattern[FormulaF]]("phi")
    val psi         = meta[FormulaF, Pattern[FormulaF]]("psi")
    val pattern     = (phi: Pattern[FormulaF]) --> (psi: Pattern[FormulaF])
    val formula1    = formulaGenerator.arbitrary.sample.get
    val formula2    = formulaGenerator.arbitrary.sample.get
    val unification = Map(phi -> formula1, psi -> formula2): MapUnification[Formula]
    val result      = pattern.substitute(unification)

    assert(result.isDefined)
    assert(result.get === formula1 --> formula2)
  }
}
