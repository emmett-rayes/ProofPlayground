package proofPlayground
package core.meta.propositional

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern
import core.meta.propositional.Unification

object Substitution:
  def substitute(pattern: Pattern.Formula[FormulaF], unification: Unification[FormulaF, Formula]): Option[Formula] =
    pattern match
      case pattern @ Pattern.Formula.Meta(name) => unification.get(pattern)
      case Pattern.Formula.Concrete(formula)    =>
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
