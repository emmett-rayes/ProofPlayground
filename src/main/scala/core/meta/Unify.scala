package proofPlayground
package core.meta

import core.logic.propositional.{Formula, FormulaF}
import core.meta.PatternF.meta
import core.meta.{Pattern, PatternF}
import core.{Algebra, Functor, catamorphism}

import scala.annotation.targetName

/** A successful unifier mapping meta-variables from a pattern to concrete values. */
type Unification[T] = Map[MetaVariable, T]

object Unification:
  /** Merge two unifications if they agree on shared variables, otherwise fail.
    *
    * @param fst the first unification to merge
    * @param snd the second unification to merge
    * @tparam T the type of the values produced by the unifications
    * @return Some(merged) when consistent; None on conflict
    */
  def merge[T](fst: Unification[T], snd: Unification[T]): Option[Unification[T]] =
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => fst(key) != snd(key)) then None else Some(fst ++ snd)

  /** Merge two unifications if they agree on shared variables, otherwise fail.
    *
    * This overload allows for unifications over sequences to be merged.
    *
    * If both unifications share a meta-variable, the values of the meta-variable in the second unification
    * must be already contained in the first unification.
    *
    * @param fst the first unification to merge
    * @param snd the second unification to merge
    * @tparam T the type of the values produced by the unifications
    * @return Some(merged) when consistent; None on conflict
    */
  @targetName("mergeSeq")
  def merge[T](fst: Unification[Seq[T]], snd: Unification[T]): Option[Unification[Seq[T]]] =
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => !fst(key).contains(snd(key))) then None else Some(fst ++ snd.view.mapValues(Seq(_)))

object Unify:
  import Unification.merge

  /** Type alias for a unification function that attempts to produce a unification.
    *
    * This is the carrier type for the algebras below.
    * It is a function because a unifier needs to be applied to different scrutinees.
    */
  type Unifier[T] = T => Option[Unification[T]]

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
    *
    * @tparam F The type of the formula functor used in the pattern.
    * @tparam T The type of the concrete formula used in unification.
    * @param patterns   the sequence of patterns to unify with the scrutinees
    * @param scrutinees the sequence of concrete formulas to check against
    * @return Some(unification) if a consistent unification exists; None otherwise
    */
  // noinspection ScalaDocUnknownParameter
  def unify[T, F[_]: Functor](using
    Algebra[F, Unifier[T]]
  )(patterns: Seq[Pattern[F]], scrutinees: Seq[T]): Option[Unification[Seq[T]]] =
    val emptyContext: (Unification[T], Map[Int, Int]) = (Map.empty, Map.empty)
    val unificationConcrete = patterns.zipWithIndex.foldLeft(Option(emptyContext)) { (ctx, pWithIdx) =>
      val (pattern, idx) = pWithIdx
      pattern.unfix match
        case PatternF.Meta(_)    => ctx
        case PatternF.Formula(_) =>
          for
            (unificationAcc, idxMap) <- ctx
            // we can alternatively track the last used index in the context
            last = idxMap.maxByOption(_._2).map(_._2).getOrElse(0)
            (unification, idxMatch) <-
              scrutinees.drop(last).map(unify[T, F](pattern, _)).zipWithIndex.find(_._1.isDefined)
            merged <- merge(unificationAcc, unification.get)
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

      val idxStutteringMeta = patterns.map(_.unfix).zip(patterns.map(_.unfix).drop(1)).zipWithIndex.collect {
        case ((PatternF.Meta(_), PatternF.Meta(_)), idx) => idx + 1
      }

      val unificationSeq = unification.map(p => p._1 -> Seq(p._2)).withDefaultValue(Seq.empty)
      patterns.zipWithIndex.foldLeft(unificationSeq) { (unification, pWithIdx) =>
        val (pattern, idx) = pWithIdx
        pattern.unfix match
          case p @ PatternF.Meta(_) if !idxStutteringMeta.contains(idx) =>
            val before = idxConcreteBefore.get(idx).flatMap(idxMap.get).getOrElse(-1)
            val after  = idxConcreteAfter.get(idx).flatMap(idxMap.get).getOrElse(scrutinees.size)
            unification + (p -> scrutinees.slice(before + 1, after))
          case _ => unification
      }

  /** Attempt to unify a pattern with a concrete formula.
    *
    * The result is a mapping from meta-variable appearing in the pattern to concrete values when a match exists.
    *
    * Rules:
    * - Meta variables in the pattern match any formula and are bound to it.
    * - Concrete constructors must structurally match and recursively unify.
    * - Leaf cases (e.g. Variable, True, False) succeed only if equal; otherwise fail.
    * - For unary connectives (e.g. ¬), the argument must unify.
    * - For binary connectives (e.g. ∧, ∨, →), both sides must unify and their substitutions must be consistent.
    *
    * @param pattern   the pattern to unify with the scrutinee
    * @param scrutinee the concrete formula to match against the pattern
    * @tparam T the type of the concrete formula used in unification
    * @tparam F the type of the formula functor used in the pattern
    * @return Some(unification) if a consistent unification exists; None otherwise
    */
  def unify[T, F[_]: Functor](using Algebra[F, Unifier[T]])(pattern: Pattern[F], scrutinee: T): Option[Unification[T]] =
    unify(pattern)(scrutinee)

  def unify[T, F[_]: Functor](using Algebra[F, Unifier[T]])(pattern: Pattern[F]): Unifier[T] =
    val algebra = Pattern.algebra[Unifier[T], F](summon) { pattern => scrutinee =>
      Some(Map(meta(pattern.name) -> scrutinee))
    }
    catamorphism(pattern)(algebra)

  /** An algebra that reduces a [[Formula]] to a `Unifier[Formula]`. */
  given Algebra[FormulaF, Unifier[Formula]] = {
    formula =>
      // noinspection DuplicatedCode
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
              merged <- merge(lhs, rhs)
            yield merged
          case (FormulaF.Disjunction(disjunction), FormulaF.Disjunction(pattern)) =>
            for
              lhs    <- pattern.lhs(disjunction.lhs)
              rhs    <- pattern.rhs(disjunction.rhs)
              merged <- merge(lhs, rhs)
            yield merged
          case (FormulaF.Implication(implication), FormulaF.Implication(pattern)) =>
            for
              lhs    <- pattern.lhs(implication.lhs)
              rhs    <- pattern.rhs(implication.rhs)
              merged <- merge(lhs, rhs)
            yield merged
          case _ => None
  }
