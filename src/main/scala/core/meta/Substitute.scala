package proofPlayground
package core.meta

import core.meta.Pattern.given
import core.meta.PatternF.{concrete, substitution}
import core.meta.Unify
import core.proof.natural.Judgement
import core.{Algebra, Functor, traverse}

import scala.language.implicitConversions

trait Substitute[T, F[_]] extends Unify[T, F] {
  extension (self: Self[Pattern[F]])
    /** Substitutes meta-variables in the pattern according to the provided unification.
      *
      * @param unification The unification mapping meta-variables to concrete formulas.
      * @return Some(substituted) if the substitution is successful; None otherwise.
      */
    def substitute(unification: Unification[T]): Option[Self[T]]
}

object Substitute {
  /** Substitutes meta-variables in the judgement according to the provided unifications.
    *
    * Meta-variables are replaced by concrete formulas if they are present in the unification.
    * Otherwise, the meta-variable is left unchanged.
    *
    * @tparam T The type of the concrete formula used in substitution.
    * @tparam F The type of the formula functor used in the pattern.
    * @param judgement    The judgement in which to perform the substitution.
    * @param unification  The unification mapping meta-variables to concrete formulas.
    * @param assumptionUnification The unification mapping assumption meta-variables to sequences of concrete formulas.
    * @param freeUnification The unification mapping free meta-variables to sequences of concrete formulas.
    */
  def substitutePartial[T: AsPattern[F], F[_]: Functor](
    judgement: Judgement[Pattern[F]],
    unification: MapUnification[T],
    assumptionUnification: MapUnification[Seq[T]],
    freeUnification: MapUnification[Seq[T]],
  ): Judgement[Pattern[F]] = {
    val assertion   = substitutePartial[T, F](judgement.assertion, unification)
    val assumptions = substitutePartial[T, F](judgement.assumptions.toSeq, assumptionUnification)
    val free        = substitutePartial[T, F](judgement.free.toSeq, freeUnification)
    Judgement(assertion, assumptions, free)
  }

  /** Substitutes meta-variables in the pattern according to the provided unification.
    *
    * Meta-variables are replaced by concrete formulas if they are present in the unification.
    * Otherwise, the meta-variable is left unchanged.
    *
    * @tparam T The type of the concrete formula used in substitution.
    * @tparam F The type of the formula functor used in the pattern.
    * @param pattern     The pattern in which to perform the substitution.
    * @param unification The unification mapping meta-variables to concrete formulas.
    *
    * @return The pattern with meta-variables substituted where possible.
    */
  def substitutePartial[T: AsPattern[F], F[_]: Functor](
    pattern: Pattern[F],
    unification: MapUnification[T]
  ): Pattern[F] = {
    def substitute(pattern: Pattern[F], unification: MapUnification[Pattern[F]]): Pattern[F] =
      pattern.unfix match {
        case pattern @ PatternF.Meta(_) =>
          unification.getOrElse(pattern, pattern)
        case PatternF.Substitution(variable, replacement, formula) =>
          substitution(
            substitute(variable, unification),
            substitute(replacement, unification),
            substitute(formula, unification),
          )
        case PatternF.Formula(formula) =>
          concrete(formula.map(substitute(_, unification)))
      }
    val patternUnification = unification.view.mapValues(_.asPattern).toMap
    substitute(pattern, patternUnification)
  }

  def substitutePartial[T: AsPattern[F], F[_]: Functor](
    patterns: Seq[Pattern[F]],
    unification: MapUnification[Seq[T]]
  ): Seq[Pattern[F]] = {
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
