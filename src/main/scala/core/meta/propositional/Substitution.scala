package proofPlayground
package core.meta.propositional

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.meta.propositional.Unification
import core.meta.{Pattern, PatternF}

object Substitution:
  def substitute(pattern: Pattern[FormulaF], unification: Unification): Option[Formula] =
    pattern.unfix match
      case pattern@PatternF.Meta(name) => unification.get(pattern)
      case PatternF.Concrete(formula) =>
        formula match
          case FormulaF.Variable(variable) =>
            Some(Formula(FormulaF.Variable(variable)))
          case FormulaF.True(_) =>
            Some(Formula(tru))
          case FormulaF.False(_) =>
            Some(Formula(fls))
          case FormulaF.Negation(negation) =>
            substitute(negation.arg, unification).map(arg => Formula(~arg))
          case FormulaF.Conjunction(conjunction) =>
            for
              lhs <- substitute(conjunction.lhs, unification)
              rhs <- substitute(conjunction.rhs, unification)
            yield Formula(lhs /\ rhs)
          case FormulaF.Disjunction(disjunction) =>
            for
              lhs <- substitute(disjunction.lhs, unification)
              rhs <- substitute(disjunction.rhs, unification)
            yield Formula(lhs \/ rhs)
          case FormulaF.Implication(implication) =>
            for
              lhs <- substitute(implication.lhs, unification)
              rhs <- substitute(implication.rhs, unification)
            yield Formula(lhs --> rhs)
