package proofPlayground
package core.meta

import core.logic.propositional.FormulaF.{fls, tru, unary_~, variable}
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.TestUnification.{arbitraryGen, asPattern}

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite

/** Tests for the [[Unification]] functions. */
class TestUnification extends AnyFunSuite:

  test("identical formulas unify") {
    val formula = TestUnification.arbitraryGen.arbitrary.sample.get
    val pattern = formula.asPattern
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("meta-variables unify any propositional formula") {
    val metavariable = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = metavariable
    val formula = TestUnification.arbitraryGen.arbitrary.sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(metavariable -> formula))
  }

  test("true pattern unifies true formula") {
    val pattern = Pattern.Formula.Concrete(tru)
    val formula = Formula(tru)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
  }

  test("true pattern does not unifies anything that is not true formula") {
    val pattern = Pattern.Formula.Concrete(tru)
    val formula = arbitraryGen.arbitrary.filter(f => f != Formula(tru)).sample.get
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
    val formula = arbitraryGen.arbitrary.filter(f => f != Formula(fls)).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
  }

  test("negation patterns unify negation formulas") {
    val subPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = Pattern.Formula.Concrete[FormulaF](~subPattern)
    val subFormula = arbitraryGen.arbitrary.sample.get
    val formula = Formula(~subFormula)
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(subPattern -> subFormula))
  }

  test("negation patterns do not unify non-negation formulas") {
    val subPattern = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = Pattern.Formula.Concrete[FormulaF](~subPattern)
    val formula = arbitraryGen.arbitrary.filter(f =>
      f.formula match
        case FormulaF.Negation(_) => false
        case _ => true
    ).sample.get
    val result = Unification.unify(pattern, formula)

    assert(result.isEmpty)
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
  given arbitraryGen: Arbitrary[Formula] = Arbitrary(
    Gen.sized(n => TestUnification.genFormula(math.min(n, MAX_FORMULA_DEPTH)))
  )

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

  extension (formula: Formula)
    /** Converts a concrete `Formula` into a `Pattern.Formula.Concrete[FormulaF]`.
     *
     * This allows using concrete formulas directly in pattern matching tests.
     *
     * @return a `Pattern.Formula.Concrete` wrapping the formula
     */
    def asPattern: Pattern.Formula.Concrete[FormulaF] =
      formula.formula match
        case FormulaF.Variable(variable) =>
          FormulaF.Variable(variable)
        case FormulaF.True(tru) =>
          FormulaF.True(tru)
        case FormulaF.False(fls) =>
          FormulaF.False(fls)
        case FormulaF.Negation(negation) =>
          FormulaF.Negation(symbol.Negation(negation.arg.asPattern))
        case FormulaF.Conjunction(conjunction) =>
          FormulaF.Conjunction(symbol.Conjunction(conjunction.lhs.asPattern, conjunction.rhs.asPattern))
        case FormulaF.Disjunction(disjunction) =>
          FormulaF.Disjunction(symbol.Disjunction(disjunction.lhs.asPattern, disjunction.rhs.asPattern))
        case FormulaF.Implication(implication) =>
          FormulaF.Implication(symbol.Implication(implication.lhs.asPattern, implication.rhs.asPattern))
