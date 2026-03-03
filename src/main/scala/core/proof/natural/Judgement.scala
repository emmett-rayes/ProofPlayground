package proofPlayground
package core.proof.natural

import core.{Algebra, Functor}
import core.meta.Pattern.given
import core.meta.Unify.given
import core.meta.Substitute.given
import core.meta.SubstitutePartial.given
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
  Unify,
}
import core.proof.SideCondition

/** Representation of a judgement in natural deduction.
  *
  * A judgement consists of a sequence of assumptions and an assertion.
  * A judgement in natural deduction has a single formula in the conclusion.
  *
  * @tparam F the type of formulas.
  * @param assertion     the formula that is asserted.
  * @param assumptions   the collection of formulas assumed to be true.
  * @param sidecondition a variable that is not allowed to appear free in the open leaves
  *                       of the derivation of this judgement, or in one of the assumptions,
  *                       depending on the rule that introduced the side condition.
  */
case class Judgement[F](assertion: F, assumptions: Seq[F], nonfree: Seq[F])(
  val sidecondition: Option[Judgement.NonFreeSideCondition[F]]
)

object Judgement {
  def apply[F](assertion: F, assumptions: Seq[F], nonfree: Seq[F]): Judgement[F] =
    new Judgement(assertion, assumptions, nonfree)(None)

  /** [[Functor]] instance for [[Judgement]]. */
  given Functor[Judgement] {
    extension [A](judgement: Judgement[A]) {
      override def map[B](f: A => B): Judgement[B] =
        Judgement(
          f(judgement.assertion),
          judgement.assumptions.map(f),
          judgement.nonfree.map(f),
        )(judgement.sidecondition.map(_.map(f)))
    }
  }

  /** [[MetaVars]] instance for [[Judgement]]. */
  given [F: MetaVars] => Judgement[F] is MetaVars {
    extension (judgement: Judgement[F]) {
      override def metavariables: Set[MetaVariable] =
        judgement.assertion.metavariables
    }
  }

  /** [[SideCondition]] instance for [[Judgement]]. */
  given [F: FreeVars] => Judgement[F] is SideCondition[F] {
    extension (judgement: Judgement[F]) {
      override def open: Boolean =
        !judgement.assumptions.contains(judgement.assertion)

      override def violations: Seq[F] =
        // if the judgement is closed, the side condition trivially holds, therefore we exclude all nonfree variables,
        // if the judgement is open exclude any nonfree variables that were introduced in this judgement,
        // since the side condition applies to free variables in the derivation of the judgement
        // we only remove the first ocurrence of the nonfree variable, in case it was already introduced before
        val exclude = if !judgement.open then judgement.nonfree
        else
          judgement.sidecondition.toSeq.collect {
            case NonFreeSideCondition.OpenLeaves(nf) => Seq(nf)
          }.flatten

        // if the side condition refers to the assertion,
        // include any nonfree variables that were introduced in this judgement
        val include = judgement.sidecondition.toSeq.collect {
          case NonFreeSideCondition.Assertion(nf) => Seq(nf)
        }.flatten

        val nonfree = judgement.nonfree.diff(exclude).union(include)
        judgement.assertion.freevariables.filter(nonfree.contains(_)).toSeq
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
          nonfreeUnification     <- MapUnification.merge(unification._3, assertionUnification)
        yield (assertionUnification, assumptionsUnification, nonfreeUnification)
      }

    extension (judgement: Judgement[Pattern[F]])
      override def unifier: Unifier = { scrutinee =>
        for {
          assertionUnification        <- judgement.assertion.unifier(scrutinee.assertion)
          assumptionsUnification      <- judgement.assumptions.unifier(scrutinee.assumptions)
          nonfreeUnification          <- judgement.nonfree.unifier(scrutinee.nonfree)
          mergedAssumptionUnification <- MapUnification.merge(assumptionsUnification, assertionUnification)
          mergedNonfreeUnification    <- MapUnification.merge(nonfreeUnification, assertionUnification)
        } yield (assertionUnification, mergedAssumptionUnification, mergedNonfreeUnification)
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
        val assertion     = judgement.assertion.substitutePartial(unification._1)
        val assumptions   = judgement.assumptions.toSeq.substitutePartial(unification._2)
        val nonfree       = judgement.nonfree.toSeq.substitutePartial(unification._3)
        val sidecondition = judgement.sidecondition.map(_.map(_.substitutePartial(unification._1)))
        Judgement(assertion, assumptions, nonfree)(sidecondition)
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
          assertion     <- judgement.assertion.substitute(unification._1)
          assumptions   <- judgement.assumptions.toSeq.substitute(unification._2)
          nonfree       <- judgement.nonfree.toSeq.substitute(unification._3)
          sidecondition <- judgement.sidecondition match
            case None     => Some(None)
            case Some(sc) => sc.traverse(_.substitute(unification._1)).map(Some(_))
        yield Judgement(assertion, assumptions, nonfree)(sidecondition)
  }

  /** Used as an intermediate type while constructing [[Judgement]] using DSL methods. */
  opaque type DSLContext[F] = (nonfreeHead: Option[NonFreeSideCondition[F]], nonfree: Seq[F], assumptions: Seq[F])

  enum NonFreeSideCondition[F] {
    case OpenLeaves(nf: F)
    case Assertion(nf: F)

    def nonfree: F = this match {
      case OpenLeaves(nf) => nf
      case Assertion(nf)  => nf
    }

    def map[F2](f: F => F2): NonFreeSideCondition[F2] = this match {
      case OpenLeaves(nf) => OpenLeaves(f(nf))
      case Assertion(nf)  => Assertion(f(nf))
    }

    def traverse[F2](f: F => Option[F2]): Option[NonFreeSideCondition[F2]] = this match {
      case OpenLeaves(nf) => f(nf).map(OpenLeaves(_))
      case Assertion(nf)  => f(nf).map(Assertion(_))
    }
  }

  opaque type DSLContextNonFree[F] = (Option[NonFreeSideCondition[F]], Seq[F])

  extension [F](assumptions: Seq[F]) {

    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, assumptions, Seq.empty)(None)
  }

  extension [F](nonfree: Seq[F]) {

    /** Infix operator for combining assumptions and nonfree sequences */
    def %(assumptions: Seq[F]): DSLContext[F] = (None, nonfree, assumptions)

    /** Infix operator for constructing nonfree sequences while keeping track of the new element */
    def ++(head: F): DSLContextNonFree[F] = (Some(NonFreeSideCondition.OpenLeaves(head)), nonfree)

    /** Infix operator for constructing nonfree sequences while keeping track of the new element */
    def ##(head: F): DSLContextNonFree[F] = (Some(NonFreeSideCondition.Assertion(head)), nonfree)
  }

  extension [F](nonfree: DSLContextNonFree[F]) {

    /** Infix operator for combining assumptions and nonfree sequences */
    def %(assumptions: Seq[F]): DSLContext[F] = (nonfree._1, nonfree._2, assumptions)
  }

  extension [F](context: DSLContext[F]) {

    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] =
      val nonfree = (context.nonfree ++ context.nonfreeHead.map(_.nonfree).toSeq)
      Judgement(assertion, context.assumptions, nonfree)(context.nonfreeHead)
  }
}
