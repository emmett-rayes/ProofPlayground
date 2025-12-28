package proofPlayground
package core.meta

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol

import org.scalacheck.{Arbitrary, Gen}

object FormulaGenerationUtil:

  /** Maximum depth for generated formulas.
    */
  inline val MAX_FORMULA_DEPTH = 5

  /** Arbitrary generator for propositional formulas used in tests.
    *
    * Uses a size-bounded recursive generator to produce a formula.
    */
  given arbitraryGenerator: Arbitrary[Formula] = Arbitrary(
    Gen.sized(n => generateFormula(math.min(n, MAX_FORMULA_DEPTH)))
  )

  /** Size-bounded recursive generator for `Formula`.
    *
    * Generates a mix of leaf formulas (variable, ⊤, ⊥) and composite formulas (¬, ∧, ∨, →). When `depth` is 0 or less,
    * only leaf constructors are produced. Otherwise, it probabilistically chooses between leaf and combined
    * constructors, recursively generating smaller sub-formulas.
    *
    * @param depth
    *   current recursion depth limit
    * @return
    *   a generator that yields well-formed propositional formulas
    */
  private def generateFormula(depth: Int): Gen[Formula] =
    lazy val leaf: Gen[Formula] = Gen.oneOf(Formula(variable()), Formula(tru), Formula(fls))

    if depth <= 0 then leaf
    else
      val sub = generateFormula(depth - 1)
      Gen.frequency(
        1 -> leaf,
        2 -> sub.map(f => Formula(FormulaF.Negation(symbol.Negation(f)))),
        3 -> Gen.zip(sub, sub).map { case (l, r) => Formula(l /\ r) },
        3 -> Gen.zip(sub, sub).map { case (l, r) => Formula(l \/ r) },
        3 -> Gen.zip(sub, sub).map { case (l, r) => Formula(l --> r) }
      )
