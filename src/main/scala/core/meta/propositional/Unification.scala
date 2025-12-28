package proofPlayground
package core.meta.propositional

import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern

/**
 * A successful unifier mapping meta variables from a pattern to concrete values.
 *
 * @tparam F functor of the formula structure (e.g. [[FormulaF]])
 * @tparam X value bound to meta variables (e.g. [[Formula]])
 */
type Unification[F[_], X] = Map[Pattern.Formula.Meta[F], X]

/**
 * Unification utilities for matching a meta-level pattern of formulas
 * against a concrete formula. The result is a mapping from meta-variables
 * appearing in the pattern to concrete formulas when a match exists.
 */
object Unification:
  /**
   * The result of attempting to unify a pattern with a formula. None denotes unification failure.
   */
  type UnificationResult[F[_], X] = Option[Unification[F, X]]

  /**
   * Attempt to unify a formula pattern with a concrete formula.
   *
   * Rules:
   * - Meta variables in the pattern match any formula and are bound to it.
   * - Concrete constructors must structurally match and recursively unify.
   * - Leaf cases (Variable, True, False) succeed only if equal; otherwise fail.
   * - For binary connectives (∧, ∨, →), both sides must unify and their substitutions must be consistent.
   *
   * @param pattern meta/structured pattern to match
   * @param formula concrete formula to check against
   * @return Some(unifier) if a consistent substitution exists; None otherwise
   */
  def unify(pattern: Pattern.Formula[FormulaF], formula: Formula): UnificationResult[FormulaF, Formula] =
    pattern match
      case Pattern.Formula.Meta(name) =>
        Some(Map(Pattern.Formula.Meta(name) -> formula))
      case Pattern.Formula.Concrete(pattern) =>
        (pattern, formula.unfix) match
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

  /**
   * Merge two unifiers if they agree on shared variables; otherwise fail.
   *
   * @return Some(merged) when consistent; None on conflict
   */
  private def mergeUnification[F[_], X](fst: Unification[F, X], snd: Unification[F, X]): UnificationResult[F, X] =
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => fst(key) != snd(key)) then None else Some(fst ++ snd)
