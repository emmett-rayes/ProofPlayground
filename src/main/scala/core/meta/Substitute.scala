package proofPlayground
package core.meta

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern.given
import core.meta.PatternF.{concrete, substitution}
import core.proof.natural.Judgement
import core.{Algebra, Functor, catamorphism, traverse}

import scala.language.implicitConversions

object Substitute {
  /** Substitutes meta-variables in the judgement according to the provided unifications.
    *
    * @tparam T The type of the concrete formula used in substitution.
    * @tparam F The type of the formula functor used in the pattern.
    * @param judgement    The judgement in which to perform the substitution.
    * @param unification  The unification mapping meta-variables to concrete formulas.
    * @param assumptionUnification The unification mapping assumption meta-variables to sequences of concrete formulas.
    * @param freeUnification The unification mapping free meta-variables to sequences of concrete formulas.
    */
  def substitute[T: CaptureAvoidingSub, F[_]: Functor](using
    Algebra[F, Option[T]]
  )(
    judgement: Judgement[Pattern[F]],
    unification: Unification[T],
    assumptionUnification: Unification[Seq[T]],
    freeUnification: Unification[Seq[T]],
  ): Option[Judgement[T]] =
    for
      assertion   <- substitute[T, F](judgement.assertion, unification)
      assumptions <- substitute[T, F](judgement.assumptions.toSeq, assumptionUnification)
      free        <- substitute[T, F](judgement.free.toSeq, freeUnification)
    yield Judgement(assertion, assumptions, free)

  /** Substitutes meta-variables in the sequence of patterns according to the provided unification.
    *
    * The substitution is performed in the order of the patterns.
    * The meta-variables are replaced by a sequence of concrete formulas.
    *
    * @tparam T The type of the concrete formula used in substitution.
    * @tparam F The type of the formula functor used in the pattern.
    * @param patterns    The sequence of patterns in which to perform the substitution.
    * @param unification The unification mapping meta-variables to concrete formulas.
    * @return Some(substituted) if the substitution is successful; None otherwise.
    */
  def substitute[T: CaptureAvoidingSub, F[_]: Functor](using
    Algebra[F, Option[T]]
  )(patterns: Seq[Pattern[F]], unification: Unification[Seq[T]]): Option[Seq[T]] =
    patterns.traverse { pattern =>
      pattern.unfix match {
        case pattern @ PatternF.Meta(name) =>
          unification.get(pattern)
        case PatternF.Substitution(variable, replacement, formula) =>
          // substitution pattern with sequence meta-variables are not supported
          None
        case PatternF.Formula(formula) =>
          // substituting with the empty unification converts a pattern without variables to a formula
          substitute[T, F](pattern, Map.empty).map(Seq(_))
      }
    }.map(_.flatten)

  /** Substitutes meta-variables in the pattern according to the provided unification.
    *
    * @tparam T The type of the concrete formula used in substitution.
    * @tparam F The type of the formula functor used in the pattern.
    * @param pattern     The pattern in which to perform the substitution.
    * @param unification The unification mapping meta-variables to concrete formulas.
    * @return Some(substituted) if the substitution is successful; None otherwise.
    */
  def substitute[T: CaptureAvoidingSub, F[_]: Functor](using
    Algebra[F, Option[T]]
  )(pattern: Pattern[F], unification: Unification[T]): Option[T] = {
    val algebra = PatternF.algebra[Option[T], F](summon) {
      case pattern @ PatternF.Meta(_) =>
        unification.get(pattern)
      case PatternF.Substitution(variable, replacement, formula) =>
        for
          variable    <- variable
          replacement <- replacement
          formula     <- formula
        yield formula.substituteWithoutCapturing(variable, replacement)
    }
    catamorphism(pattern)(algebra)
  }

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
    unification: Unification[T],
    assumptionUnification: Unification[Seq[T]],
    freeUnification: Unification[Seq[T]],
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
  def substitutePartial[T: AsPattern[F], F[_]: Functor](pattern: Pattern[F], unification: Unification[T]): Pattern[F] = {
    def substitute(pattern: Pattern[F], unification: Unification[Pattern[F]]): Pattern[F] =
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
    unification: Unification[Seq[T]]
  ): Seq[Pattern[F]] = {
    def substitute(patterns: Seq[Pattern[F]], unification: Unification[Seq[Pattern[F]]]): Seq[Pattern[F]] =
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
    val patternUnification: Unification[Seq[Pattern[F]]] = unification.view.mapValues(_.map(_.asPattern)).toMap
    substitute(patterns, patternUnification)
  }

  /** An algebra for collapsing a [[Formula]] into an `Option[Formula]`. */
  given Algebra[FormulaF, Option[Formula]] {
    override def apply(formula: FormulaF[Option[Formula]]): Option[Formula] = {
      formula match {
        case FormulaF.Variable(sym)            => Some(variable(sym))
        case FormulaF.True(_)                  => Some(tru)
        case FormulaF.False(_)                 => Some(fls)
        case FormulaF.Negation(negation)       => negation.arg.map(arg => ~arg)
        case FormulaF.Conjunction(conjunction) => for lhs <- conjunction.lhs; rhs <- conjunction.rhs yield lhs /\ rhs
        case FormulaF.Disjunction(disjunction) => for lhs <- disjunction.lhs; rhs <- disjunction.rhs yield lhs \/ rhs
        case FormulaF.Implication(implication) => for lhs <- implication.lhs; rhs <- implication.rhs yield lhs --> rhs
        case FormulaF.Universal(universal)     =>
          for variable <- universal.variable; body <- universal.body yield forall(variable, body)
        case FormulaF.Existential(existential) =>
          for variable <- existential.variable; body <- existential.body yield exists(variable, body)
      }
    }
  }
}
