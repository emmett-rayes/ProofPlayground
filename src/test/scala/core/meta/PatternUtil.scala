package proofPlayground
package core.meta

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.Pattern.given
import core.meta.{Pattern, PatternF}

import scala.language.implicitConversions

object PatternUtil:
  given Conversion[FormulaF[Pattern[FormulaF]], Pattern[FormulaF]] =
    PatternF.concrete(_)

  extension (formula: Formula)
    /** Converts a concrete `Formula` into a `PatternF.Concrete[FormulaF]`.
      *
      * This allows using concrete formulas directly in pattern matching tests.
      *
      * @return A `Pattern` wrapping the formula
      */
    def asPattern: Pattern[FormulaF] =
      formula.unfix match
        case FormulaF.Variable(symbol)         => variable(symbol)
        case FormulaF.True(_)                  => tru
        case FormulaF.False(_)                 => fls
        case FormulaF.Negation(negation)       => ~negation.arg.asPattern
        case FormulaF.Conjunction(conjunction) => conjunction.lhs.asPattern /\ conjunction.rhs.asPattern
        case FormulaF.Disjunction(disjunction) => disjunction.lhs.asPattern \/ disjunction.rhs.asPattern
        case FormulaF.Implication(implication) => implication.lhs.asPattern --> implication.rhs.asPattern
