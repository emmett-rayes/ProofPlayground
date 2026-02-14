package proofPlayground
package core.meta

import core.logic.propositional.FormulaF
import core.meta.Pattern
import core.proof.Inference
import core.proof.natural.Judgement
import core.{Algebra, Functor, catamorphism}

/** A typeclass for extracting meta-variables from a container. */
trait MetaVars:
  type Self

  extension (self: Self)
    /** Returns the set of meta-variables appearing in `self`. */
    def metavariables: Set[MetaVariable]

object MetaVars:
  /** Extracts the set of meta-variables appearing in a pattern.
    *
    * @param pattern The pattern to extract meta-variables from.
    * @tparam F The type of the formula functor used in `pattern`.
    *
    * @return The set of meta-variables appearing in `pattern`.
    */
  def metavariables[F[_]: Functor](using Algebra[F, Set[MetaVariable]])(pattern: Pattern[F]): Set[MetaVariable] =
    val algebra = Pattern.algebra[Set[MetaVariable], F](summon)(Set(_))
    catamorphism(pattern)(algebra)

  /** Algebra for collapsing a [[FormulaF]] to a [[Set]] of values without producing any information at the leaves.
    *
    * @tparam T The type of values produced by the algebra.
    */
  given [T] => Algebra[FormulaF, Set[T]] = {
    case FormulaF.Variable(variable)       => Set.empty
    case FormulaF.True(tru)                => Set.empty
    case FormulaF.False(fls)               => Set.empty
    case FormulaF.Negation(negation)       => negation.arg
    case FormulaF.Conjunction(conjunction) => conjunction.lhs ++ conjunction.rhs
    case FormulaF.Disjunction(disjunction) => disjunction.lhs ++ disjunction.rhs
    case FormulaF.Implication(implication) => implication.lhs ++ implication.rhs
  }

  /** Instance of [[MetaVars]] for [[Pattern]]. */
  given [F[_]: Functor] => (Algebra[F, Set[MetaVariable]]) => Pattern[F] is MetaVars:
    extension (pattern: Pattern[F])
      override def metavariables: Set[MetaVariable] = MetaVars.metavariables(pattern)

  /** Instance of [[MetaVars]] for [[Judgement]]. */
  given [F: MetaVars] => Judgement[F] is MetaVars:
    extension (judgement: Judgement[F])
      override def metavariables: Set[MetaVariable] =
        judgement.assertion.metavariables ++ judgement.assumptions.flatMap(_.metavariables)

  /** Instance of [[MetaVars]] for [[Inference]]. */
  given [J: MetaVars] => Inference[J] is MetaVars:
    extension (inference: Inference[J])
      override def metavariables: Set[MetaVariable] =
        inference.conclusion.metavariables ++ inference.premises.flatMap(_.metavariables)
