package proofPlayground
package core.proof.natural

import core.Fix
import core.{Algebra, Functor}
import core.meta.AsPattern
import core.meta.Pattern.given
import core.meta.PatternF
import core.meta.Unify.given
import core.meta.Substitute.given
import core.meta.SubstitutePartial
import core.meta.SubstitutePartial.given
import core.meta.{
  CaptureAvoidingSub,
  FreeVars,
  MapUnification,
  MapUnifier,
  MetaVariable,
  MetaVars,
  Pattern,
  Substitute,
  Unify,
}
import core.proof.SideCondition

/** Representation of a judgement in natural deduction.
  *
  * A judgement consists of a sequence of assumptions and an assertion.
  * A judgement in natural deduction has a single formula in the conclusion.
  *
  * @tparam F the type of formulas.
  * @param assertion   the formula that is asserted.
  * @param assumptions the collection of formulas assumed to be true.
  * @param free the collection of variables that are not allowed to appear in the conclusion.
  *             this is used for the side conditions of existential and universal quantifiers.
  */
case class Judgement[F](assertion: F, assumptions: Seq[F], free: Seq[F])

case object Judgement {

  /** Used as an intermediate type while constructing [[Judgement]] using DSL methods. */
  opaque type DSLContext[F] = (Seq[F], Seq[F])

  /** [[Functor]] instance for [[Judgement]]. */
  given Functor[Judgement] {
    extension [A](judgement: Judgement[A]) {
      override def map[B](f: A => B): Judgement[B] =
        Judgement(f(judgement.assertion), judgement.assumptions.map(f), judgement.free.map(f))
    }
  }

  /** [[MetaVars]] instance for [[Judgement]]. */
  given [F: MetaVars] => Judgement[F] is MetaVars {
    extension (judgement: Judgement[F]) {
      override def metavariables: Set[MetaVariable] =
        judgement.assertion.metavariables ++ judgement.assumptions.flatMap(_.metavariables)
    }
  }

  /** [[SideCondition]] instance for [[Judgement]]. */
  given [F: FreeVars] => Judgement[F] is SideCondition[F] {
    extension (judgement: Judgement[F]) {
      override def violations: Seq[F] =
        judgement.free.collect {
          case free if judgement.assertion.freevariables.contains(free) => free
        }
    }
  }

  /** [[Unify]] instance for [[Judgement]]. */
  given [T, F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Judgement is Unify[T, F] {
    override type Unification = [X] =>> (MapUnification[X], MapUnification[Seq[X]], MapUnification[Seq[X]])

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] = {
        for
          assertionUnification   <- MapUnification.merge(unification._1, aux)
          assumptionsUnification <- MapUnification.merge(unification._2, assertionUnification)
          freeUnification        <- MapUnification.merge(unification._3, assertionUnification)
        yield (assertionUnification, assumptionsUnification, freeUnification)
      }

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier = { scrutinee =>
        for {
          assertionUnification        <- judgement.assertion.unifier(scrutinee.assertion)
          assumptionsUnification      <- judgement.assumptions.unifier(scrutinee.assumptions)
          freeUnification             <- judgement.free.unifier(scrutinee.free)
          mergedAssumptionUnification <- MapUnification.merge(assumptionsUnification, assertionUnification)
          mergedFreeUnification       <- MapUnification.merge(freeUnification, assertionUnification)
        } yield (assertionUnification, mergedAssumptionUnification, mergedFreeUnification)
      }
  }

  /** [[SubstitutePartial]] instance for [[Judgement]]. */
  given [T: AsPattern[F], F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Judgement is SubstitutePartial[T, F] {
    override type Unification = JudgementUnify.Unification
    private val JudgementUnify = Judgement.given_is_Judgement_Unify

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] =
        JudgementUnify.merge(unification)(aux)

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier =
        JudgementUnify.unifier(judgement)

    extension (judgement: Judgement[Pattern[F]])
      override def substitutePartial(unification: Unification[T]): Judgement[Pattern[F]] =
        val assertion   = judgement.assertion.substitutePartial(unification._1)
        val assumptions = judgement.assumptions.toSeq.substitutePartial(unification._2)
        val free        = judgement.free.toSeq.substitutePartial(unification._3)
        Judgement(assertion, assumptions, free)
  }

  /** [[Substitute]] instance for [[Judgement]]. */
  given [T: {AsPattern[F], CaptureAvoidingSub}, F[_]: Functor]
    => (Algebra[F, Option[T]])
    => (Algebra[F, MapUnifier[T]])
      => Judgement is Substitute[T, F] {

    override type Unification = JudgementSubstitutePartial.Unification
    private val JudgementSubstitutePartial = Judgement.given_is_Judgement_SubstitutePartial

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] =
        JudgementSubstitutePartial.merge(unification)(aux)

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier =
        JudgementSubstitutePartial.unifier(judgement)

    extension (judgement: Judgement[Pattern[F]])
      override def substitutePartial(unification: Unification[T]): Judgement[Pattern[F]] =
        JudgementSubstitutePartial.substitutePartial(judgement)(unification)

    extension (judgement: Judgement[Pattern[F]])
      override def substitute(unification: Unification[T]): Option[Judgement[T]] =
        for
          assertion   <- judgement.assertion.substitute(unification._1)
          assumptions <- judgement.assumptions.toSeq.substitute(unification._2)
          free        <- judgement.free.toSeq.substitute(unification._3)
        yield Judgement(assertion, assumptions, free)
  }

  extension [F](assumptions: Seq[F]) {

    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, assumptions, Seq.empty)
  }

  extension [F](free: Seq[F]) {

    /** Infix operator for combining assumptions and free sequences */
    def %(assumptions: Seq[F]): DSLContext[F] = (free, assumptions)
  }

  extension [F](context: DSLContext[F]) {

    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, context._2, context._1)
  }
}
