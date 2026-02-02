package proofPlayground
package core.meta

import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern
import core.proof.Inference
import core.proof.natural.Judgement
import core.{Algebra, Fix, Functor, catamorphism, meta}

/** A typeclass for extracting meta-variables from a container. */
trait MetaVarsWithoutFunctor:
  type Self

  extension (self: Self)
    /** Returns the set of meta-variables appearing in `self`. */
    def metavariables: Set[MetaVariable]

/** A typeclass for extracting meta-variables from a fixed point of a functor. */
trait MetaVars extends MetaVarsWithoutFunctor:
  override type Self

  /** The functor of the fixed point. */
  type Functor[_]

  /** The algebra for extracting meta-variables for [[Functor]] with carrier type `Set[MetaVariable]`. */
  def algebra(functor: Functor[Set[MetaVariable]]): Set[MetaVariable]

object MetaVars:
  /** Instance of [[MetaVars]] for [[Formula]]. */
  given Formula is MetaVars:
    override type Functor = FormulaF

    extension (formula: Formula)
      override def metavariables: Set[MetaVariable] =
        catamorphism(formula)(algebra)

    def algebra(formula: FormulaF[Set[MetaVariable]]): Set[MetaVariable] =
      formula match
        case FormulaF.Variable(variable)       => Set.empty
        case FormulaF.True(tru)                => Set.empty
        case FormulaF.False(fls)               => Set.empty
        case FormulaF.Negation(negation)       => negation.arg
        case FormulaF.Conjunction(conjunction) => conjunction.lhs ++ conjunction.rhs
        case FormulaF.Disjunction(disjunction) => disjunction.lhs ++ disjunction.rhs
        case FormulaF.Implication(implication) => implication.lhs ++ implication.rhs

  /** Instance of [[MetaVars]] for [[Pattern]]. */
  given [F[_]: Functor] => (fix: Fix[F] is MetaVars { type Functor = F }) => Pattern[F] is MetaVars:
    override type Functor = [T] =>> PatternF[F, T]

    extension (pattern: Pattern[F])
      override def metavariables: Set[MetaVariable] =
        catamorphism(pattern)(algebra)

    def algebra(pattern: PatternF[F, Set[MetaVariable]]): Set[MetaVariable] =
      val subalgebra = fix.algebra
      pattern match
        case pattern @ PatternF.Meta(_) => Set(pattern)
        case PatternF.Formula(formula)  => subalgebra(formula)

  /** Instance of [[MetaVarsWithoutFunctor]] for [[Judgement]]. */
  given [F: MetaVarsWithoutFunctor] => Judgement[F] is MetaVarsWithoutFunctor:
    extension (judgement: Judgement[F])
      override def metavariables: Set[MetaVariable] =
        judgement.assertion.metavariables ++ judgement.assumptions.flatMap(_.metavariables)

  /** Instance of [[MetaVarsWithoutFunctor]] for [[Inference]]. */
  given [J: MetaVarsWithoutFunctor] => Inference[J] is MetaVarsWithoutFunctor:
    extension (inference: Inference[J])
      override def metavariables: Set[MetaVariable] =
        inference.conclusion.metavariables ++ inference.hypotheses.flatMap(_.metavariables)
