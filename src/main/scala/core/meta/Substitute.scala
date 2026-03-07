package proofPlayground
package core.meta

import scala.language.implicitConversions

import core.meta.Pattern.given
import core.meta.PatternF.{concrete, substitution}
import core.meta.Unify
import core.proof.natural.Judgement
import core.{Algebra, Functor, traverse}

trait SubstitutePartial[T, F[_]] extends Unify[T, F] {
  extension (self: Self[Pattern[F]])
    /** Substitutes meta-variables in the pattern according to the provided unification.
      *
      * Meta-variables are replaced by concrete formulas if they are present in the unification.
      * Otherwise, the meta-variable is left unchanged.
      *
      * @param unification The unification mapping meta-variables to concrete formulas.
      * @return The pattern with meta-variables substituted where possible.
      */
    def substitutePartial(unification: Unification[T]): Self[Pattern[F]]
}

trait Substitute[T, F[_]] extends SubstitutePartial[T, F] {
  extension (self: Self[Pattern[F]])
    /** Substitutes meta-variables in the pattern according to the provided unification.
      *
      * @param unification The unification mapping meta-variables to concrete formulas.
      * @return Some(substituted) if the substitution is successful; None otherwise.
      */
    def substitute(unification: Unification[T]): Option[Self[T]]
}

object SubstitutePartial {

  /** [[SubstitutePartial]] instance for [[Seq]]. */
  given [T: AsPattern[F], F[_]: Functor]
    => (Algebra[F, MapUnifier[T]])
      => Seq is SubstitutePartial[T, F] {

    override type Unification = SeqUnify.Unification
    private val SeqUnify = Unify.given_is_Seq_Unify

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): UnificationResult[Unification[T]] =
        SeqUnify.merge(unification)(aux)

    extension (patterns: Seq[Pattern[F]])
      override def unifier: Unifier =
        SeqUnify.unifier(patterns)

    extension (patterns: Seq[Pattern[F]])
      override def substitutePartial(unification: Unification[T]): Seq[Pattern[F]] =
        val patternUnification = unification.view.mapValues(_.map(_.asPattern)).toMap
        def substitute(patterns: Seq[Pattern[F]]): Seq[Pattern[F]] =
          patterns.flatMap { pattern =>
            pattern.unfix match {
              case pattern @ PatternF.Meta(_) =>
                patternUnification.getOrElse(pattern, Seq(pattern)): Seq[Pattern[F]]
              case _ =>
                Seq(pattern.substitutePartialSimple(unification))
            }
          }
        substitute(patterns)
  }

  /** Helper extension method to partially substitute a sequence of patterns with a unification that maps to sequences. */
  extension [T: AsPattern[F], F[_]: Functor](using Algebra[F, MapUnifier[T]])(pattern: Pattern[F]) {
    def substitutePartialSimple(unification: MapUnification[Seq[T]]): Pattern[F] =
      val simpleUnification: MapUnification[T] =
        unification.filter { (_, v) => v.size == 1 }.map { (k, v) => k -> v.head }
      pattern.substitutePartial(simpleUnification)
  }
}

object Substitute {

  /** [[Substitute]] instance for [[Seq]]. */
  given [T: {AsPattern[F], CaptureAvoidingSub}, F[_]: Functor]
    => (Algebra[F, Option[T]])
    => (Algebra[F, MapUnifier[T]])
      => Seq is Substitute[T, F] {
    override type Unification = SeqSubstitutePartial.Unification
    private val SeqSubstitutePartial = SubstitutePartial.given_is_Seq_SubstitutePartial

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): UnificationResult[Unification[T]] =
        SeqSubstitutePartial.merge(unification)(aux)

    extension (patterns: Seq[Pattern[F]])
      override def unifier: Unifier =
        SeqSubstitutePartial.unifier(patterns)

    extension (patterns: Seq[Pattern[F]])
      override def substitutePartial(unification: Unification[T]): Seq[Pattern[F]] =
        SeqSubstitutePartial.substitutePartial(patterns)(unification)

    extension (patterns: Seq[Pattern[F]])
      override def substitute(unification: Unification[T]): Option[Seq[T]] =
        patterns.traverse { pattern =>
          pattern.unfix match {
            case pattern @ PatternF.Meta(name) =>
              unification.get(pattern)
            case _ =>
              pattern.substituteSimple(unification).map(Seq(_))
          }
        }.map(_.flatten)
  }

  /** Helper extension method to substitute a sequence of patterns with a unification that maps to sequences. */
  extension [T: {AsPattern[F], CaptureAvoidingSub}, F[_]: Functor](using
    Algebra[F, Option[T]],
    Algebra[F, MapUnifier[T]],
  )(pattern: Pattern[F]) {
    def substituteSimple(unification: MapUnification[Seq[T]]): Option[T] =
      val simpleUnification: MapUnification[T] =
        unification.filter { (_, v) => v.size == 1 }.map { (k, v) => k -> v.head }
      pattern.substitute(simpleUnification)
  }
}
