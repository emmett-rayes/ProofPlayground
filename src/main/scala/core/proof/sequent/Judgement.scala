package proofPlayground
package core.proof.sequent

import core.Fix
import core.{Algebra, Functor}
import core.meta.MapUnification.given
import core.meta.Pattern.given
import core.meta.SeqUnification.{*, given}
import core.meta.Substitute.{*, given}
import core.meta.SubstitutePartial.{*, given}
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
  UnificationResult,
  Unify,
}
import core.proof.{ClosedQuery, Proof, SideCondition}

/** Representation of a judgement in sequent calculus.
  *
  * A judgement consists of a sequence of antecedents and a sequence of succedents.
  *
  * @tparam F the type of formulas.
  * @param antecedents the collection of formulas that are assumed to be true.
  * @param succedents the collection of formulas that of which one is asserted to be true.
  */
case class Judgement[F](antecedents: Seq[F], succedents: Seq[F])

type JudgementUnification = SeqUnification

object Judgement {

  /** [[Functor]] instance for [[Judgement]]. */
  given Functor[Judgement] {
    extension [A](judgement: Judgement[A]) {
      override def map[B](f: A => B): Judgement[B] =
        Judgement(judgement.antecedents.map(f), judgement.succedents.map(f))
    }
  }

  /** [[MetaVars]] instance for [[Judgement]]. */
  given [F: MetaVars] => Judgement[F] is MetaVars {
    extension (judgement: Judgement[F]) {
      override def metavariables: Set[MetaVariable] =
        judgement.antecedents.flatMap(_.metavariables).toSet ++
          judgement.succedents.flatMap(_.metavariables).toSet
    }
  }

  /** [[ClosedQuery]] instance for [[Judgement]]. */
  given [F] => Judgement[F] is ClosedQuery {
    extension (judgement: Judgement[F]) {
      override def closed: Boolean = false // a sequent judgement is always open until explicitly closed by an axiom
    }
  }

  /** [[Unify]] instance for [[Judgement]]. */
  given [T, F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Judgement is Unify[T, F] {
    override type Uni = JudgementUnification

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier = {
        scrutinee =>
          for {
            antecendentsUnification <- judgement.antecedents.unifier(scrutinee.antecedents)
            succedentsUnification   <- judgement.succedents.unifier(scrutinee.succedents)
            mergedUnification       <- antecendentsUnification.merge(succedentsUnification)
          } yield mergedUnification
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
        val antecedents = judgement.antecedents.substitutePartial(unification)
        val succedents  = judgement.succedents.substitutePartial(unification)
        Judgement(antecedents, succedents)
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
          antecedents <- judgement.antecedents.substitute(unification)
          succedents  <- judgement.succedents.substitute(unification)
        yield Judgement(antecedents, succedents)
  }

  extension [F](antecedents: Seq[F]) {

    /** Judgement infix constructor. */
    def |-(succedents: Seq[F]): Judgement[F] = Judgement(antecedents, succedents)
  }

  // hack: using judgement as a pattern container to allow easy substitution
  def sidecondition[T](variable: T)(
    condition: [F[_]] => (Fix[F] is FreeVars) ?=> (Fix[F], Proof[F, Judgement]) => Boolean
  ) =
    SideCondition(Judgement(Seq.empty, Seq(variable))) { [f[_]] => (_) ?=> (judgement, proof) =>
      condition(judgement.succedents.head, proof)
    }
}
