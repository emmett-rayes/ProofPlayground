package proofPlayground
package core.meta

import core.meta.Pattern.given
import core.{Algebra, Functor}

/** Result of a unification attempt.
  *
  * A unification result always carries a value:
  * - [[UnificationResult.success]] stores a successful unification.
  * - [[UnificationResult.failure]] stores a best-effort partial unification.
  */
opaque type UnificationResult[T] = Either[T, T]

object UnificationResult {

  /** Construct a successful unification result. */
  def success[T](result: T): UnificationResult[T] = Right(result)

  /** Construct a failed unification result with its partial payload. */
  def failure[T](result: T): UnificationResult[T] = Left(result)

  extension [T](result: UnificationResult[T]) {

    /** Return the payload regardless of success or failure. */
    def get: T = result.merge

    /** True when the result represents successful unification. */
    def isSuccess: Boolean = result.isRight

    /** True when the result represents failed unification. */
    def isFailure: Boolean = result.isLeft

    /** Convert to [[Option]], dropping failure payload information. */
    def toOption: Option[T] = result.toOption

    def flatMap[U](f: T => UnificationResult[U]): UnificationResult[U] =
      result match {
        case Right(t) => f(t)
        case Left(t)  => Left(f(t).merge)
      }

    def map[U](f: T => U): UnificationResult[U] =
      result match {
        case Right(t) => Right(f(t))
        case Left(t)  => Left(f(t))
      }
  }
}

/** A commonly used unifier type from scrutinees to unifications over scrutinees.
  *
  * The unifier returns [[UnificationResult.success]] for a consistent match and
  * [[UnificationResult.failure]] with partial context when matching fails.
  */
type MapUnifier[T] = T => UnificationResult[MapUnification[T]]

/** A unification mapping meta-variables from a pattern to concrete values. */
type MapUnification[T] = Map[MetaVariable, T]

object MapUnification:
  import scala.annotation.targetName

  /** Merge two unifications if they agree on shared variables, otherwise fail.
    *
    * @param fst the first unification to merge
    * @param snd the second unification to merge
    * @tparam T the type of the values produced by the unifications
    * @return [[UnificationResult.success]](merged) when consistent;
    *         [[UnificationResult.failure]](mergedWithoutConflicts) when conflicts are found
    */
  def merge[T](fst: MapUnification[T], snd: MapUnification[T]): UnificationResult[MapUnification[T]] = {
    val conflicts = fst.keySet.intersect(snd.keySet).filter { key => fst(key) != snd(key) }
    if conflicts.isEmpty
    then
      UnificationResult.success(fst ++ snd)
    else
      val fstFiltered = fst.filterNot { case (k, _) => conflicts.contains(k) }
      val sndFiltered = snd.filterNot { case (k, _) => conflicts.contains(k) }
      UnificationResult.failure(fstFiltered ++ sndFiltered)
  }

  /** Merge two unifications if they agree on shared variables, otherwise fail.
    *
    * This overload allows for unifications over sequences to be merged.
    *
    * @param fst the first unification to merge
    * @param snd the second unification to merge
    * @tparam T the type of the values produced by the unifications
    * @return [[UnificationResult.success]](merged) when consistent;
    *         [[UnificationResult.failure]](mergedWithoutConflicts) when conflicts are found
    */
  @targetName("mergeSeq")
  def merge[T](fst: MapUnification[Seq[T]], snd: MapUnification[T]): UnificationResult[MapUnification[Seq[T]]] = {
    merge[Seq[T]](fst, snd.map { (k, v) => k -> Seq(v) })
  }

/** Type alias for a unifier function that attempts to produce a unification.
  *
  * This is the carrier type for the unification algebras.
  * It is a function because a unifier can be applied to different scrutinees.
  */
trait Unify[T, F[_]] {
  type Self[_]
  type Unification[_]
  type Unifier = Self[T] => UnificationResult[Unification[T]]

  extension (unification: Unification[T])
    def merge(aux: MapUnification[T]): UnificationResult[Unification[T]]

  extension (self: Self[Pattern[F]])
    def unifier: Unifier
}

object Unify {

  /** [[Unify]] instance for [[Seq]]. */
  given [T, F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Seq is Unify[T, F] {
    override type Unification = [X] =>> MapUnification[Seq[X]]

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): UnificationResult[Unification[T]] =
        MapUnification.merge(unification, aux)

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
      * @return [[UnificationResult.success]](unification) if a consistent unification exists;
      *         [[UnificationResult.failure]](partialUnification) otherwise
      */
    extension (patterns: Seq[Pattern[F]])
      override def unifier: Unifier = {
        scrutinees =>
          {
            val emptyContext: (MapUnification[T], Map[Int, Int]) = (Map.empty, Map.empty)
            val unificationConcrete                              =
              patterns.zipWithIndex.foldLeft(Option(emptyContext)) { (ctx, pWithIdx) =>
                val (pattern, idx) = pWithIdx
                pattern.unfix match {
                  case PatternF.Meta(_)               => ctx
                  case PatternF.Substitution(_, _, _) => ctx
                  case PatternF.Formula(_)            =>
                    for
                      (unificationAcc, idxMap) <- ctx
                      // we can alternatively track the last used index in the context
                      last = idxMap.maxByOption(_._2).map(_._2).getOrElse(0)
                      (unification, idxMatch) <-
                        scrutinees.drop(last).map(pattern.unifier(_)).zipWithIndex.findLast(_._1.isSuccess)
                      merged <- MapUnification.merge(unificationAcc, unification.get).toOption
                    yield (merged, idxMap + (idx -> idxMatch))
                }
              }

            val resultUnificationWithIdxMap =
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
                  patterns.zipWithIndex.reverse.foldLeft((Map.empty[Int, Int], patterns.size - 1)) {
                    (ctx, pWithIdx) =>
                    val (map, lastConcreteIdx) = ctx
                    val (pattern, idx)         = pWithIdx
                    pattern.unfix match {
                      case PatternF.Meta(_) | PatternF.Substitution(_, _, _) =>
                        (map + (idx -> lastConcreteIdx), lastConcreteIdx)
                      case PatternF.Formula(_) => (map + (idx -> idx), idx)
                    }
                  }._1

                val idxStutteringMeta =
                  patterns.map(_.unfix).zip(patterns.map(_.unfix).drop(1)).zipWithIndex.collect {
                  case ((PatternF.Meta(_), PatternF.Meta(_)), idx) => idx + 1
                }

                val unificationSeq    = unification.map(p => p._1 -> Seq(p._2)).withDefaultValue(Seq.empty)
                val unificationResult =
                  patterns.zipWithIndex.foldLeft(UnificationResult.success(unificationSeq)) {
                    (unification, pWithIdx) =>
                  val (pattern, idx) = pWithIdx
                  pattern.unfix match {
                        case p @ PatternF.Meta(_) =>
                          if idxStutteringMeta.contains(idx) then
                            // cast safety: see `idxStutteringMeta` construction
                            UnificationResult.failure(
                              unification.get.removed(patterns(idx - 1).unfix.asInstanceOf[p.type])
                            )
                          else
                      val before = idxConcreteBefore.get(idx).flatMap(idxMap.get).getOrElse(-1)
                      val after  = idxConcreteAfter.get(idx).flatMap(idxMap.get).getOrElse(scrutinees.size)
                            unification.map(_ + (p -> scrutinees.slice(before + 1, after)))
                    case _ => unification
                  }
                }
                (unificationResult, idxMap)
              }

            resultUnificationWithIdxMap.map {
              case (resultUnification, idxMap) =>
                // check that all scrutinees are matched by some meta-variable in the unification
                val complete = scrutinees.zipWithIndex
                  .filterNot { case (_, idx) => idxMap.values.exists(_ == idx) }
                  .forall { case (scrutinee, _) => resultUnification.get.values.exists(_.contains(scrutinee)) }
                if complete then resultUnification else UnificationResult.failure(resultUnification.get)
            }.getOrElse(UnificationResult.failure(Map.empty))
          }
      }
  }
}
