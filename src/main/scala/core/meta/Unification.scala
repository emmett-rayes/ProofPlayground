package proofPlayground
package core.meta

import core.logic.propositional.{Formula, FormulaF}
import core.meta.PatternF.meta
import core.meta.{Pattern, PatternF}
import core.{Algebra, catamorphism}

/** A successful unifier mapping meta variables from a pattern to concrete values. */
type Unification[T] = Map[PatternF.Meta[?, ?], T]

/** Unification utilities for matching a pattern against a concrete formula.
  * The result is a mapping from meta-variables
  * appearing in the pattern to concrete formulas when a match exists.
  */
object Unification:
  /** Type alias for a unification function that attempts to produce a unification.
    *
    * This is the carrier type for the algebras below.
    * It is a function because a unifier needs to be applied to different scrutinees.
    */
  private type Unifier[T] = T => Option[Unification[T]]

  /** Attempt to unify a formula pattern with a concrete formula.
    *
    * Rules:
    * - Meta variables in the pattern match any formula and are bound to it.
    * - Concrete constructors must structurally match and recursively unify.
    * - Leaf cases (Variable, True, False) succeed only if equal; otherwise fail.
    * - For binary connectives (∧, ∨, →), both sides must unify and their substitutions must be consistent.
    *
    * @param pattern meta/structured pattern to match
    * @param scrutinee concrete formula to check against
    * @return Some(unifier) if a consistent substitution exists; None otherwise
    */
  def unify(pattern: Pattern[FormulaF], scrutinee: Formula): Option[Unification[Formula]] =
    catamorphism(pattern)(algebra[FormulaF, Formula](algebra))(scrutinee)

  private def algebra[F[_], T](subalgebra: Algebra[F, Unifier[T]])(pattern: PatternF[F, Unifier[T]]): Unifier[T] =
    pattern match
      case PatternF.Meta(name)       => scrutinee => Some(Map(meta(name) -> scrutinee))
      case PatternF.Formula(formula) => subalgebra(formula)

  private def algebra(formula: FormulaF[Unifier[Formula]]): Unifier[Formula] =
    scrutinee =>
      (scrutinee.unfix, formula) match
        case (FormulaF.Variable(variable), FormulaF.Variable(pattern)) =>
          if pattern == variable then Some(Map.empty) else None
        case (FormulaF.True(_), FormulaF.True(_)) =>
          Some(Map.empty)
        case (FormulaF.False(_), FormulaF.False(_)) =>
          Some(Map.empty)
        case (FormulaF.Negation(negation), FormulaF.Negation(pattern)) =>
          pattern.arg(negation.arg)
        case (FormulaF.Conjunction(conjunction), FormulaF.Conjunction(pattern)) =>
          for
            lhs    <- pattern.lhs(conjunction.lhs)
            rhs    <- pattern.rhs(conjunction.rhs)
            merged <- mergeUnification(lhs, rhs)
          yield merged
        case (FormulaF.Disjunction(disjunction), FormulaF.Disjunction(pattern)) =>
          for
            lhs    <- pattern.lhs(disjunction.lhs)
            rhs    <- pattern.rhs(disjunction.rhs)
            merged <- mergeUnification(lhs, rhs)
          yield merged
        case (FormulaF.Implication(implication), FormulaF.Implication(pattern)) =>
          for
            lhs    <- pattern.lhs(implication.lhs)
            rhs    <- pattern.rhs(implication.rhs)
            merged <- mergeUnification(lhs, rhs)
          yield merged
        case _ => None

  /** Merge two unifiers if they agree on shared variables, otherwise fail.
    *
    * @return Some(merged) when consistent; None on conflict
    */
  private def mergeUnification[T](fst: Unification[T], snd: Unification[T]): Option[Unification[T]] =
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => fst(key) != snd(key)) then None else Some(fst ++ snd)
