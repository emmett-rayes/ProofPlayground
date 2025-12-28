package proofPlayground
package core.meta

import core.logic.propositional.{Formula, FormulaF}

object Unification:
  def unify(
    pattern: Pattern.Formula[FormulaF],
    formula: Formula
  ): Option[Map[Pattern.Formula.Meta[FormulaF], Formula]] =
    pattern match
      case Pattern.Formula.Meta(name) =>
        Some(Map(Pattern.Formula.Meta(name) -> formula))
      case Pattern.Formula.Concrete(pattern) =>
        (pattern, formula.formula) match
          case (FormulaF.Variable(variablePattern), FormulaF.Variable(variable)) if variablePattern == variable =>
            Some(Map.empty)
          case (FormulaF.True(_), FormulaF.True(_)) =>
            Some(Map.empty)
          case (FormulaF.False(_), FormulaF.False(_)) =>
            Some(Map.empty)
          case (FormulaF.Negation(negationPattern), FormulaF.Negation(negation)) =>
            unify(negationPattern.arg, negation.arg)
            // TODO
          case (FormulaF.Conjunction(conjunctionPattern), FormulaF.Conjunction(conjunction)) =>
            unify(conjunctionPattern.lhs, conjunction.lhs)
            unify(conjunctionPattern.rhs, conjunction.rhs)
            // TODO
          case (FormulaF.Disjunction(disjunctionPattern), FormulaF.Disjunction(disjunction)) =>
            unify(disjunctionPattern.lhs, disjunction.lhs)
            unify(disjunctionPattern.rhs, disjunction.rhs)
            // TODO
          case (FormulaF.Implication(implicationPattern), FormulaF.Implication(implication)) =>
            unify(implicationPattern.lhs, implication.lhs)
            unify(implicationPattern.rhs, implication.rhs)
          // TODO
          case _ => None
