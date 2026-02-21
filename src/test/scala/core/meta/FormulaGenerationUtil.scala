package proofPlayground
package core.meta

import org.scalacheck.{Arbitrary, Gen}

import scala.language.implicitConversions

import core.logic.propositional.Formula
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*

object FormulaGenerationUtil {

  /** Maximum depth for generated formulas. */
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
    * Generates a mix of leaf formulas (variable, ⊤, ⊥) and composite formulas (¬, ∧, ∨, →).
    * When `depth` is 0 or less, only leaf constructors are produced.
    * Otherwise, it probabilistically chooses between leaf and combined constructors,
    * recursively generating smaller sub-formulas.
    *
    * @param depth Recursion depth limit
    * @return A generator that yields well-formed propositional formulas
    */
  private def generateFormula(depth: Int): Gen[Formula] = {
    def randomId() = Gen.alphaStr.sample.get

    lazy val leaf = Gen.oneOf[Formula](variable(randomId()), tru, fls)

    if depth <= 0 then leaf
    else {
      val sub = generateFormula(depth - 1)
      Gen.frequency(
        1 -> leaf,
        2 -> sub.map(f => ~f),
        3 -> Gen.zip(sub, sub).map { case (l, r) => l /\ r },
        3 -> Gen.zip(sub, sub).map { case (l, r) => l \/ r },
        3 -> Gen.zip(sub, sub).map { case (l, r) => l --> r }
      )
    }
  }
}
