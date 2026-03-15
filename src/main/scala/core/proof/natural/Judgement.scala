package proofPlayground
package core.proof.natural

import core.Fix
import core.{Algebra, Functor}
import core.meta.MapUnification.given
import core.meta.Pattern.given
import core.meta.SeqUnification.given
import core.meta.Substitute.given
import core.meta.SubstitutePartial.given
import core.meta.Unify.given
import core.meta.{
  AsPattern,
  CaptureAvoidingSub,
  FreeVars,
  MapUnification,
  MapUnifier,
  MetaVariable,
  MetaVars,
  Pattern,
  SeqUnification,
  Substitute,
  SubstitutePartial,
  Unification,
  UnificationResult,
  Unify,
}
import core.proof.{ClosedQuery, Proof, SideCondition}

/** Representation of a judgement in natural deduction.
  *
  * A judgement consists of a sequence of assumptions and an assertion.
  * A judgement in natural deduction has a single formula in the conclusion.
  *
  * @tparam F the type of formulas.
  * @param assertion     the formula that is asserted.
  * @param assumptions   the collection of formulas assumed to be true.
  */
case class Judgement[F](assertion: F, assumptions: Seq[F])

type JudgementUnification[T] = (MapUnification[T], SeqUnification[T])

object JudgementUnification {

  /** [[Unification]] instance for [[JudgementUnification]]. */
  given JudgementUnification is Unification {
    override def empty[T]: JudgementUnification[T] = (Map.empty, Map.empty)

    extension [T](unification: JudgementUnification[T]) {
      override def update(aux: MapUnification[T]): UnificationResult[JudgementUnification[T]] =
        for
          assertionUnification   <- unification._1.merge(aux)
          assumptionsUnification <- unification._2.update(assertionUnification)
        yield (assertionUnification, assumptionsUnification)

      override def merge(other: JudgementUnification[T]): UnificationResult[JudgementUnification[T]] =
        for
          assertionUnification   <- unification._1.merge(other._1)
          assumptionsUnification <- unification._2.merge(other._2)
        yield (assertionUnification, assumptionsUnification)
    }
  }
}

object Judgement {
  import JudgementUnification.given

  /** [[Functor]] instance for [[Judgement]]. */
  given Functor[Judgement] {
    extension [A](judgement: Judgement[A]) {
      override def map[B](f: A => B): Judgement[B] =
        Judgement(f(judgement.assertion), judgement.assumptions.map(f))
    }
  }

  /** [[MetaVars]] instance for [[Judgement]]. */
  given [F: MetaVars] => Judgement[F] is MetaVars {
    extension (judgement: Judgement[F]) {
      override def metavariables: Set[MetaVariable] =
        judgement.assertion.metavariables
    }
  }

  /** [[ClosedQuery]] instance for [[Judgement]]. */
  given [F] => Judgement[F] is ClosedQuery {
    extension (judgement: Judgement[F]) {
      override def closed: Boolean =
        judgement.assumptions.contains(judgement.assertion)
    }
  }

  /** [[Unify]] instance for [[Judgement]]. */
  given [T, F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Judgement is Unify[T, F] {
    override type Uni = JudgementUnification

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier = {
        scrutinee =>
          for {
            assertionUnification        <- judgement.assertion.unifier(scrutinee.assertion)
            assumptionsUnification      <- judgement.assumptions.unifier(scrutinee.assumptions)
            mergedAssumptionUnification <- assumptionsUnification.update(assertionUnification)
          } yield (assertionUnification, mergedAssumptionUnification)
      }
  }

  /** [[SubstitutePartial]] instance for [[Judgement]]. */
  given [T: AsPattern[F], F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Judgement is SubstitutePartial[T, F] {
    override type Uni = JudgementUnification

    private val JudgementUnify = Judgement.given_is_Judgement_Unify

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier =
        JudgementUnify.unifier(judgement)

    extension (judgement: Judgement[Pattern[F]])
      override def substitutePartial(unification: Uni[T]): Judgement[Pattern[F]] =
        val assertion   = judgement.assertion.substitutePartial(unification._1)
        val assumptions = judgement.assumptions.toSeq.substitutePartial(unification._2)
        Judgement(assertion, assumptions)
  }

  /** [[Substitute]] instance for [[Judgement]]. */
  given [T: {AsPattern[F], CaptureAvoidingSub}, F[_]: Functor]
    => (Algebra[F, Option[T]])
    => (Algebra[F, MapUnifier[T]])
      => Judgement is Substitute[T, F] {

    override type Uni = JudgementUnification

    private val JudgementSubstitutePartial = Judgement.given_is_Judgement_SubstitutePartial

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier =
        JudgementSubstitutePartial.unifier(judgement)

    extension (judgement: Judgement[Pattern[F]])
      override def substitutePartial(unification: Uni[T]): Judgement[Pattern[F]] =
        JudgementSubstitutePartial.substitutePartial(judgement)(unification)

    extension (judgement: Judgement[Pattern[F]])
      override def substitute(unification: Uni[T]): Option[Judgement[T]] =
        for
          assertion   <- judgement.assertion.substitute(unification._1)
          assumptions <- judgement.assumptions.toSeq.substitute(unification._2)
        yield Judgement(assertion, assumptions)
  }

  extension [F](assumptions: Seq[F]) {

    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, assumptions)
  }

  // hack: using judgement as a pattern container to allow easy substitution
  def sidecondition[T](variable: T)(
    condition: [F[_]] => (Fix[F] is FreeVars) ?=> (Fix[F], Proof[F, Judgement]) => Boolean
  ) =
    SideCondition(Judgement(variable, Seq.empty)) { [f[_]] => (_) ?=> (judgement, proof) =>
      condition(judgement.assertion, proof)
    }
}
