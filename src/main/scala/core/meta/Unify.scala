package proofPlayground
package core.meta

import core.{Algebra, Functor, catamorphism}

import scala.annotation.targetName

/** A successful unifier mapping meta-variables from a pattern to concrete values. */
type Unification[T] = Map[MetaVariable, T]

/** Type alias for a unifier function that attempts to produce a unification.
  *
  * This is the carrier type for the unification algebras.
  * It is a function because a unifier can be applied to different scrutinees.
  */
type Unifier[T] = T => Option[Unification[T]]

trait Unify[T] {
  type Self

  extension (self: Self)
    def unifier: Unifier[T]
}

object Unification {

  /** Merge two unifications if they agree on shared variables, otherwise fail.
    *
    * @param fst the first unification to merge
    * @param snd the second unification to merge
    * @tparam T the type of the values produced by the unifications
    * @return Some(merged) when consistent; None on conflict
    */
  def merge[T](fst: Unification[T], snd: Unification[T]): Option[Unification[T]] = {
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => fst(key) != snd(key)) then None else Some(fst ++ snd)
  }

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
  def merge[T](fst: Unification[Seq[T]], snd: Unification[T]): Option[Unification[Seq[T]]] = {
    val intersection = fst.keySet.intersect(snd.keySet)
    if intersection.exists(key => !fst(key).contains(snd(key))) then None else Some(fst ++ snd.view.mapValues(Seq(_)))
  }
}

object Unify {
  import Unification.merge
  import core.meta.Pattern.given

  /** [[Unify]] instance for `Seq[Pattern[F]]`. */
  given [T, F[_]: Functor] => (Algebra[F, Unifier[T]]) => Seq[Pattern[F]] is Unify[Seq[T]] {

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
      * @return Some(unification) if a consistent unification exists; None otherwise
      */
    extension (patterns: Seq[Pattern[F]])
      override def unifier: Unifier[Seq[T]] = {
        scrutinees =>
          {
            val emptyContext: (Unification[T], Map[Int, Int]) = (Map.empty, Map.empty)
            val unificationConcrete = patterns.zipWithIndex.foldLeft(Option(emptyContext)) { (ctx, pWithIdx) =>
              val (pattern, idx) = pWithIdx
              pattern.unfix match {
                case PatternF.Meta(_)               => ctx
                case PatternF.Substitution(_, _, _) => ctx // substitution patterns match anything
                case PatternF.Formula(_)            =>
                  for
                    (unificationAcc, idxMap) <- ctx
                    // we can alternatively track the last used index in the context
                    last = idxMap.maxByOption(_._2).map(_._2).getOrElse(0)
                    (unification, idxMatch) <-
                      scrutinees.drop(last).map(pattern.unifier(_)).zipWithIndex.find(_._1.isDefined)
                    merged <- merge(unificationAcc, unification.get)
                  yield (merged, idxMap + (idx -> idxMatch))
              }
            }

            for
              (unification, idxMap) <- unificationConcrete
            yield {
              val idxConcreteBefore = patterns.zipWithIndex.foldLeft((Map.empty[Int, Int], 0)) { (ctx, pWithIdx) =>
                val (map, lastConcreteIdx) = ctx
                val (pattern, idx)         = pWithIdx
                pattern.unfix match {
                  case PatternF.Meta(_) | PatternF.Substitution(_, _, _) =>
                    (map + (idx -> lastConcreteIdx), lastConcreteIdx)
                  case PatternF.Formula(_) => (map + (idx -> idx), idx)
                }
              }._1

              val idxConcreteAfter =
                patterns.zipWithIndex.reverse.foldLeft((Map.empty[Int, Int], patterns.size - 1)) { (ctx, pWithIdx) =>
                  val (map, lastConcreteIdx) = ctx
                  val (pattern, idx)         = pWithIdx
                  pattern.unfix match {
                    case PatternF.Meta(_) | PatternF.Substitution(_, _, _) =>
                      (map + (idx -> lastConcreteIdx), lastConcreteIdx)
                    case PatternF.Formula(_) => (map + (idx -> idx), idx)
                  }
                }._1

              val idxStutteringMeta = patterns.map(_.unfix).zip(patterns.map(_.unfix).drop(1)).zipWithIndex.collect {
                case ((PatternF.Meta(_), PatternF.Meta(_)), idx) => idx + 1
              }

              val unificationSeq = unification.map(p => p._1 -> Seq(p._2)).withDefaultValue(Seq.empty)
              patterns.zipWithIndex.foldLeft(unificationSeq) { (unification, pWithIdx) =>
                val (pattern, idx) = pWithIdx
                pattern.unfix match {
                  case p @ PatternF.Meta(_) if !idxStutteringMeta.contains(idx) =>
                    val before = idxConcreteBefore.get(idx).flatMap(idxMap.get).getOrElse(-1)
                    val after  = idxConcreteAfter.get(idx).flatMap(idxMap.get).getOrElse(scrutinees.size)
                    unification + (p -> scrutinees.slice(before + 1, after))
                  case _ => unification
                }
              }
            }
          }
      }
  }
}
