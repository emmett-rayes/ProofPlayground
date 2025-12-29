package proofPlayground
package core.meta.propositional

import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.{Pattern, PatternF}

object PatternUtil:
  extension (formula: Formula)
    /** Converts a concrete `Formula` into a `PatternF.Concrete[FormulaF]`.
      *
      * This allows using concrete formulas directly in pattern matching tests.
      *
      * @return A `Pattern.Formula.Concrete` wrapping the formula
      */
    def asPattern: PatternF.Formula[FormulaF, Pattern[FormulaF]] =
      formula.unfix match
        case FormulaF.Variable(variable)       => FormulaF.Variable(variable)
        case FormulaF.True(tru)                => FormulaF.True(tru)
        case FormulaF.False(fls)               => FormulaF.False(fls)
        case FormulaF.Negation(negation)       => ~Pattern(negation.arg.asPattern)
        case FormulaF.Conjunction(conjunction) =>
          Pattern(conjunction.lhs.asPattern) /\ Pattern(conjunction.rhs.asPattern)
        case FormulaF.Disjunction(disjunction) =>
          Pattern(disjunction.lhs.asPattern) \/ Pattern(disjunction.rhs.asPattern)
        case FormulaF.Implication(implication) =>
          Pattern(implication.lhs.asPattern) --> Pattern(implication.rhs.asPattern)
