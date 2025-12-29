package proofPlayground
package core.meta.propositional

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.Pattern.given
import core.meta.PatternF.{concrete, meta}
import core.meta.propositional.FormulaGenerationUtil
import core.meta.propositional.PatternUtil.asPattern
import core.meta.{Pattern, PatternF, Substitution, Unification}

import org.scalatest.funsuite.AnyFunSuite

/** Tests for [[Substitution]] functions. */
class TestSubstitution extends AnyFunSuite:
  private val formulaGenerator = FormulaGenerationUtil.arbitraryGenerator

  test("meta-variables can be substituted with any formula") {
    val pattern     = meta[FormulaF, Pattern[FormulaF]]("phi")
    val formula     = formulaGenerator.arbitrary.sample.get
    val unification = Map(pattern -> formula): Unification[Formula]
    val result      = Substitution.substitute(pattern, unification)

    assert(result.isDefined)
    assert(result.get === formula)
  }

  test("substitution fails for unbound meta-variables") {
    val pattern = meta[FormulaF, Pattern[FormulaF]]("phi")
    val result  = Substitution.substitute(pattern, Map.empty)

    assert(result.isEmpty)
  }

  test("concrete patterns are not affected by substitution") {
    val formula = formulaGenerator.arbitrary.sample.get
    val pattern = formula.asPattern
    val result  = Substitution.substitute(pattern, Map.empty)

    assert(result.isDefined)
    assert(result.get === formula)
  }

  test("variable pattern is not affected by substitution") {
    val variableSymbol = symbol.Variable[FormulaF.Propositional]()
    val pattern        = concrete[FormulaF, Pattern[FormulaF]](variable(variableSymbol))
    val result         = Substitution.substitute(pattern, Map.empty)

    assert(result.isDefined)
    assert(result.get === Formula(FormulaF.Variable(variableSymbol)))
  }

  test("true pattern is not affected by substitution") {
    val pattern = concrete[FormulaF, Pattern[FormulaF]](tru)
    val result  = Substitution.substitute(pattern, Map.empty)

    assert(result.isDefined)
    assert(result.get === Formula(tru))
  }

  test("false pattern is not affected by substitution") {
    val pattern = concrete[FormulaF, Pattern[FormulaF]](fls)
    val result  = Substitution.substitute(pattern, Map.empty)

    assert(result.isDefined)
    assert(result.get === Formula(fls))
  }

  test("conjunction propagates substitutions") {
    val phi         = meta[FormulaF, Pattern[FormulaF]]("phi")
    val psi         = meta[FormulaF, Pattern[FormulaF]]("psi")
    val pattern     = PatternF.Formula[FormulaF, Pattern[FormulaF]](Pattern(phi) /\ Pattern(psi))
    val formula1    = formulaGenerator.arbitrary.sample.get
    val formula2    = formulaGenerator.arbitrary.sample.get
    val unification = Map(phi -> formula1, psi -> formula2): Unification[Formula]
    val result      = Substitution.substitute(pattern, unification)

    assert(result.isDefined)
    assert(result.get === Formula(formula1 /\ formula2))
  }

  test("disjunction propagates substitutions") {
    val phi         = meta[FormulaF, Pattern[FormulaF]]("phi")
    val psi         = meta[FormulaF, Pattern[FormulaF]]("psi")
    val pattern     = PatternF.Formula[FormulaF, Pattern[FormulaF]](Pattern(phi) \/ Pattern(psi))
    val formula1    = formulaGenerator.arbitrary.sample.get
    val formula2    = formulaGenerator.arbitrary.sample.get
    val unification = Map(phi -> formula1, psi -> formula2): Unification[Formula]
    val result      = Substitution.substitute(pattern, unification)

    assert(result.isDefined)
    assert(result.get === Formula(formula1 \/ formula2))
  }

  test("implication propagates substitutions") {
    val phi         = meta[FormulaF, Pattern[FormulaF]]("phi")
    val psi         = meta[FormulaF, Pattern[FormulaF]]("psi")
    val pattern     = PatternF.Formula[FormulaF, Pattern[FormulaF]](Pattern(phi) --> Pattern(psi))
    val formula1    = formulaGenerator.arbitrary.sample.get
    val formula2    = formulaGenerator.arbitrary.sample.get
    val unification = Map(phi -> formula1, psi -> formula2): Unification[Formula]
    val result      = Substitution.substitute(pattern, unification)

    assert(result.isDefined)
    assert(result.get === Formula(formula1 --> formula2))
  }
