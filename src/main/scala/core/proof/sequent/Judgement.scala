package proofPlayground
package core.proof.sequent

import core.{Algebra, Functor}
import core.meta.Pattern.given
import core.meta.Unify.given
import core.meta.Substitute.{*, given}
import core.meta.SubstitutePartial.{*, given}
import core.meta.{
  AsPattern,
  CaptureAvoidingSub,
  FreeVars,
  MapUnification,
  MapUnifier,
  MetaVariable,
  MetaVars,
  Pattern,
  Substitute,
  SubstitutePartial,
  UnificationResult,
  Unify,
}
import core.proof.SideCondition

/** Representation of a judgement in sequent calculus.
  *
  * A judgement consists of a sequence of antecedents and a sequence of succedents.
  *
  * @tparam F the type of formulas.
  * @param antecedents the collection of formulas that are assumed to be true.
  * @param succedents the collection of formulas that of which one is asserted to be true.
  * @param sidecondition an optional side-condition, which is a pair of a variable and a collection of formulas
  *                       in which that variable cannot appear free.
  */
case class Judgement[F](antecedents: Seq[F], succedents: Seq[F])(
  val sidecondition: Option[Judgement.NonFreeSideCondition[F]]
)

object Judgement {
  type NonFreeSideCondition[F] = (variable: F, nonfree: Seq[F])

  def apply[F](antecedents: Seq[F], succedents: Seq[F]): Judgement[F] =
    new Judgement(antecedents, succedents)(None)

  /** [[Functor]] instance for [[Judgement]]. */
  given Functor[Judgement] {
    extension [A](judgement: Judgement[A]) {
      override def map[B](f: A => B): Judgement[B] =
        Judgement(judgement.antecedents.map(f), judgement.succedents.map(f))(
          judgement.sidecondition.map { sc =>
            (f(sc.variable), sc.nonfree.map(f))
          }
        )
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

  /** [[SideCondition]] instance for [[Judgement]]. */
  given [F: FreeVars] => Judgement[F] is SideCondition[F] {
    extension (judgement: Judgement[F]) {
      override def open: Boolean = true // a judgement is always open until explicitly closed by an axiom

      override def violations: Seq[F] =
        judgement.sidecondition.map { sc =>
          sc.nonfree.filter(_.freevariables.contains(sc.variable))
        }.getOrElse(Seq.empty)
    }
  }

  /** [[Unify]] instance for [[Judgement]]. */
  given [T, F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Judgement is Unify[T, F] {
    override type Unification = [X] =>> MapUnification[Seq[X]]

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): UnificationResult[Unification[T]] =
        MapUnification.merge(unification, aux)

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier = { scrutinee =>
        for {
          antecendentsUnification <- judgement.antecedents.unifier(scrutinee.antecedents)
          succedentsUnification   <- judgement.succedents.unifier(scrutinee.succedents)
          mergedUnification       <- MapUnification.merge[Seq[T]](antecendentsUnification, succedentsUnification)
        } yield mergedUnification
      }
  }

  /** [[SubstitutePartial]] instance for [[Judgement]]. */
  given [T: AsPattern[F], F[_]: Functor] => (Algebra[F, MapUnifier[T]]) => Judgement is SubstitutePartial[T, F] {
    override type Unification = JudgementUnify.Unification
    private val JudgementUnify = Judgement.given_is_Judgement_Unify

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): UnificationResult[Unification[T]] =
        JudgementUnify.merge(unification)(aux)

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier =
        JudgementUnify.unifier(judgement)

    extension (judgement: Judgement[Pattern[F]])
      override def substitutePartial(unification: Unification[T]): Judgement[Pattern[F]] =
        val antecedents   = judgement.antecedents.substitutePartial(unification)
        val succedents    = judgement.succedents.substitutePartial(unification)
        val sidecondition = judgement.sidecondition.map { sc =>
          (sc.variable.substitutePartialSimple(unification), sc.nonfree.substitutePartial(unification))
        }
        Judgement(antecedents, succedents)(sidecondition)
  }

  /** [[Substitute]] instance for [[Judgement]]. */
  given [T: {AsPattern[F], CaptureAvoidingSub}, F[_]: Functor]
    => (Algebra[F, Option[T]])
    => (Algebra[F, MapUnifier[T]])
      => Judgement is Substitute[T, F] {

    override type Unification = JudgementSubstitutePartial.Unification
    private val JudgementSubstitutePartial = Judgement.given_is_Judgement_SubstitutePartial

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): UnificationResult[Unification[T]] =
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
          antecedents   <- judgement.antecedents.substitute(unification)
          succedents    <- judgement.succedents.substitute(unification)
          sidecondition <- judgement.sidecondition match
            case None     => Some(None)
            case Some(sc) =>
              for
                variable <- sc.variable.substituteSimple(unification)
                nonfree  <- sc.nonfree.substitute(unification)
              yield Some((variable, nonfree))
        yield Judgement(antecedents, succedents)(sidecondition)
  }

  extension [F](judgement: Judgement[F]) {

    /** Add a side-condition to the judgement. */
    def -(variable: F, nonfree: F*): Judgement[F] =
      Judgement(judgement.antecedents, judgement.succedents)(Some((variable, nonfree)))
  }

  extension [F](antecedents: Seq[F]) {

    /** Judgement infix constructor. */
    def |-(succedents: Seq[F]): Judgement[F] = Judgement(antecedents, succedents)
  }
}
