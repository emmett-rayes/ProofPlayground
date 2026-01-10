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

  /** Attempt to unify a sequence of formula patterns with a sequence of concrete formulas.
    *
    * Rules:
    * - Meta variables in the patterns can match any number of scrutinees (including zero).
    * - Concrete patterns must match exactly one scrutinee.
    * - The order of scrutinees must be preserved.
    * - All unifications must be consistent across the entire sequence.
    * - If a concrete pattern cannot be matched, unification fails.
    * - Meta variables can match multiple scrutinees, and all matched scrutinees are collected.
    *
    * @note There might be multiple valid unifications that disagree on how adjacent meta-variables
    *       are assigned scrutinees. This function returns the unification where the first meta-variable
    *       is assigned as many scrutinees as possible. The following adjacent meta-variables are then assigned
    *       an empty sequence until the next concrete pattern is matched.
    * @param patterns   the sequence of patterns to match
    * @param scrutinees the sequence of concrete formulas to check against
    * @return Some(unification) if a consistent unification exists; None otherwise
    */
  def unify(patterns: Seq[Pattern[FormulaF]], scrutinees: Seq[Formula]): Option[Unification[Seq[Formula]]] =
    val emptyContext: (Unification[Formula], Map[Int, Int]) = (Map.empty, Map.empty)
    val unificationConcrete = patterns.zipWithIndex.foldLeft(Option(emptyContext)) { (ctx, pWithIdx) =>
      val (pattern, idx) = pWithIdx
      pattern.unfix match
        case PatternF.Meta(_)    => ctx
        case PatternF.Formula(_) =>
          for
            (unificationAcc, idxMap) <- ctx
            // we can alternatively track the last used index in the context
            last = idxMap.maxByOption(_._2).map(_._2).getOrElse(0)
            (unification, idxMatch) <- scrutinees.drop(last).map(unify(pattern, _)).zipWithIndex.find(_._1.isDefined)
            merged                  <- mergeUnification(unificationAcc, unification.get)
          yield (merged, idxMap + (idx -> idxMatch))
    }

    for
      (unification, idxMap) <- unificationConcrete
    yield
      val idxConcreteBefore = patterns.zipWithIndex.foldLeft((Map.empty[Int, Int], 0)) { (ctx, pWithIdx) =>
        val (map, lastConcreteIdx) = ctx
        val (pattern, idx)         = pWithIdx
        pattern.unfix match
          case PatternF.Meta(_)    => (map + (idx -> lastConcreteIdx), lastConcreteIdx)
          case PatternF.Formula(_) => (map + (idx -> idx), idx)
      }._1

      val idxConcreteAfter =
        patterns.zipWithIndex.reverse.foldLeft((Map.empty[Int, Int], patterns.size - 1)) { (ctx, pWithIdx) =>
          val (map, lastConcreteIdx) = ctx
          val (pattern, idx)         = pWithIdx
          pattern.unfix match
            case PatternF.Meta(_)    => (map + (idx -> lastConcreteIdx), lastConcreteIdx)
            case PatternF.Formula(_) => (map + (idx -> idx), idx)
        }._1

      val unificationSeq = unification.map(p => p._1 -> Seq(p._2))
      patterns.zipWithIndex.foldLeft(unificationSeq) { (unification, pWithIdx) =>
        val (pattern, idx) = pWithIdx
        pattern.unfix match
          case PatternF.Formula(_)  => unification
          case p @ PatternF.Meta(_) =>
            val before = idxConcreteBefore.get(idx).flatMap(idxMap.get).getOrElse(-1)
            val after  = idxConcreteAfter.get(idx).flatMap(idxMap.get).getOrElse(scrutinees.size)
            unification + (p -> scrutinees.slice(before + 1, after))
      }

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
    * @return Some(unification) if a consistent unification exists; None otherwise
    */
  def unify(pattern: Pattern[FormulaF], scrutinee: Formula): Option[Unification[Formula]] =
    catamorphism(pattern)(algebra[FormulaF, Formula](algebra))(scrutinee)

  /** Unification algebra for the [[PatternF]] functor with carrier `Unifier[T]`. */
  private def algebra[F[_], T](subalgebra: Algebra[F, Unifier[T]])(pattern: PatternF[F, Unifier[T]]): Unifier[T] =
    pattern match
      case PatternF.Meta(name)       => scrutinee => Some(Map(meta(name) -> scrutinee))
      case PatternF.Formula(formula) => subalgebra(formula)

  /** Unification algebra for the [[FormulaF]] functor with carrier `Unifier[Formula]`. */
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

  /** Merge two unifications if they agree on shared variables, otherwise fail.
    *
    * @return Some(merged) when consistent; None on conflict
    */
  private def mergeUnification[T](fst: Unification[T], snd: Unification[T]): Option[Unification[T]] =
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => fst(key) != snd(key)) then None else Some(fst ++ snd)
