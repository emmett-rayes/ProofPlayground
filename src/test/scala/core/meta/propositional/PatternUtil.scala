package proofPlayground
package core.meta.propositional

import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import core.meta.Pattern

object PatternUtil:
  extension (formula: Formula)
    /** Converts a concrete `Formula` into a `Pattern.Formula.Concrete[FormulaF]`.
      *
      * This allows using concrete formulas directly in pattern matching tests.
      *
      * @return
      *   a `Pattern.Formula.Concrete` wrapping the formula
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
