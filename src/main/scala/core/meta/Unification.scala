package proofPlayground
package core.meta

import core.logic.propositional.{Formula, FormulaF}

object Unification:
  type Unification[F[_], X] = Map[Pattern.Formula.Meta[F], X]
  type UnificationResult[F[_], X] = Option[Unification[F, X]]

  def unify(pattern: Pattern.Formula[FormulaF], formula: Formula): UnificationResult[FormulaF, Formula] =
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
          case (FormulaF.Conjunction(conjunctionPattern), FormulaF.Conjunction(conjunction)) =>
            for
              lhs <- unify(conjunctionPattern.lhs, conjunction.lhs)
              rhs <- unify(conjunctionPattern.rhs, conjunction.rhs)
              merged <- mergeUnification(lhs, rhs)
            yield merged
          case (FormulaF.Disjunction(disjunctionPattern), FormulaF.Disjunction(disjunction)) =>
            for
              lhs <- unify(disjunctionPattern.lhs, disjunction.lhs)
              rhs <- unify(disjunctionPattern.rhs, disjunction.rhs)
              merged <- mergeUnification(lhs, rhs)
            yield merged
          case (FormulaF.Implication(implicationPattern), FormulaF.Implication(implication)) =>
            for
              lhs <- unify(implicationPattern.lhs, implication.lhs)
              rhs <- unify(implicationPattern.rhs, implication.rhs)
              merged <- mergeUnification(lhs, rhs)
            yield merged
          case _ => None

  private def mergeUnification[F[_], X](fst: Unification[F, X], snd: Unification[F, X]): UnificationResult[F, X] =
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => fst(key) != snd(key)) then None else Some(fst ++ snd)
