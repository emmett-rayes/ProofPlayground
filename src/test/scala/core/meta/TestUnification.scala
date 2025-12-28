package proofPlayground
package core.meta

import core.logic.propositional.FormulaF.{fls, tru, variable}
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite

/** Tests for the [[Unification]] functions. */
class TestUnification extends AnyFunSuite:
  test("meta-variables unify any propositional formula") {

    val metavariable = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = metavariable
    val formula = TestUnification.arbitraryGen.arbitrary.sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(metavariable -> formula))
  }

object TestUnification:

  /**
   * Maximum depth for generated formulas.
   */
  inline val MAX_FORMULA_DEPTH = 5

  /**
   * Arbitrary generator for propositional formulas used in tests.
   *
   * Uses a size-bounded recursive generator to produce a formula.
   */
  given arbitraryGen: Arbitrary[Formula] = Arbitrary(Gen.sized(n => TestUnification.genFormula(math.min(n, MAX_FORMULA_DEPTH))))

  /**
   * Size-bounded recursive generator for `Formula`.
   *
   * Generates a mix of leaf formulas (variable, ⊤, ⊥) and composite
   * formulas (¬, ∧, ∨, →). When `depth` is 0 or less, only leaf constructors
   * are produced. Otherwise, it probabilistically chooses between leaf and combined
   * constructors, recursively generating smaller sub-formulas.
   *
   * @param depth current recursion depth limit
   * @return a generator that yields well-formed propositional formulas
   */
  private def genFormula(depth: Int): Gen[Formula] =
    lazy val leaf: Gen[Formula] = Gen.oneOf(
      Formula(variable()),
      Formula(tru),
      Formula(fls),
    )

    if depth <= 0 then leaf
    else
      val sub = genFormula(depth - 1)
      Gen.frequency(
        2 -> leaf,
        2 -> sub.map(f => Formula(FormulaF.Negation(symbol.Negation(f)))),
        3 -> Gen.zip(sub, sub).map { case (l, r) => Formula(FormulaF.Conjunction(symbol.Conjunction(l, r))) },
        3 -> Gen.zip(sub, sub).map { case (l, r) => Formula(FormulaF.Disjunction(symbol.Disjunction(l, r))) },
        3 -> Gen.zip(sub, sub).map { case (l, r) => Formula(FormulaF.Implication(symbol.Implication(l, r))) }
      )
