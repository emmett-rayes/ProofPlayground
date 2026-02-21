package proofPlayground
package core.meta

import scala.language.implicitConversions

import core.meta.Pattern.given
import core.meta.PatternF.{concrete, substitution}
import core.meta.Unify
import core.proof.natural.Judgement
import core.{Algebra, Functor, traverse}

trait Substitute[T, F[_]] extends Unify[T, F] {
  extension (self: Self[Pattern[F]])
    /** Substitutes meta-variables in the pattern according to the provided unification.
      *
      * @param unification The unification mapping meta-variables to concrete formulas.
      * @return Some(substituted) if the substitution is successful; None otherwise.
      */
    def substitute(unification: Unification[T]): Option[Self[T]]
}

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

object Substitute {

  /** [[SubstitutePartial]] instance for [[Seq]]. */
  given [T: AsPattern[F], F[_]: Functor]
    => (Algebra[F, MapUnifier[T]])
      => Seq is SubstitutePartial[T, F] {

    override type Unification = SeqUnify.Unification
    private val SeqUnify = Unify.given_is_Seq_Unify

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] =
        SeqUnify.merge(unification)(aux)

    extension (patterns: Seq[Pattern[F]])
      override def unifier: Unifier =
        SeqUnify.unifier(patterns)

    extension (patterns: Seq[Pattern[F]])
      override def substitutePartial(unification: Unification[T]): Seq[Pattern[F]] =
        def substitute(patterns: Seq[Pattern[F]], unification: MapUnification[Seq[Pattern[F]]]): Seq[Pattern[F]] =
          patterns.flatMap { pattern =>
            pattern.unfix match {
              case pattern @ PatternF.Meta(_) =>
                unification.getOrElse(pattern, Seq(pattern)): Seq[Pattern[F]]
              case PatternF.Substitution(variable, replacement, formula) =>
                // substitution pattern with sequence meta-variables are not supported
                None
              case PatternF.Formula(formula) =>
                substitute(Seq(concrete(formula)), unification)
            }
          }
        val patternUnification: MapUnification[Seq[Pattern[F]]] = unification.view.mapValues(_.map(_.asPattern)).toMap
        substitute(patterns, patternUnification)
  }

  /** [[Substitute]] instance for [[Seq]]. */
  given [T: CaptureAvoidingSub, F[_]: Functor]
    => (Algebra[F, Option[T]])
    => (Algebra[F, MapUnifier[T]])
      => Seq is Substitute[T, F] {
    override type Unification = SeqUnify.Unification
    private val SeqUnify = Unify.given_is_Seq_Unify

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] =
        SeqUnify.merge(unification)(aux)

    extension (patterns: Seq[Pattern[F]])
      override def unifier: Unifier =
        SeqUnify.unifier(patterns)

    extension (patterns: Seq[Pattern[F]])
      override def substitute(unification: Unification[T]): Option[Seq[T]] =
        patterns.traverse { pattern =>
          pattern.unfix match {
            case pattern @ PatternF.Meta(name) =>
              unification.get(pattern)
            case PatternF.Substitution(variable, replacement, formula) =>
              // substitution pattern with sequence meta-variables are not supported
              None
            case PatternF.Formula(formula) =>
              // substituting with the empty unification converts a pattern without variables to a formula
              pattern.substitute(Map.empty[MetaVariable, T]).map(Seq(_))
          }
        }.map(_.flatten)
  }
}
