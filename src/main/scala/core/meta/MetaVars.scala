package proofPlayground
package core.meta

import core.logic.propositional.FormulaF
import core.meta.Pattern
import core.proof.Inference
import core.proof.natural.Judgement
import core.{Algebra, Functor, catamorphism}

/** A typeclass for extracting meta-variables from a container. */
trait MetaVars {
  type Self

  extension (self: Self) {

    /** Returns the set of meta-variables appearing in `self`. */
    def metavariables: Set[MetaVariable]
  }
}

object MetaVars {
  /** Instance of [[MetaVars]] for [[Judgement]]. */
  given [F: MetaVars] => Judgement[F] is MetaVars {
    extension (judgement: Judgement[F]) {
      override def metavariables: Set[MetaVariable] =
        judgement.assertion.metavariables ++ judgement.assumptions.flatMap(_.metavariables)
    }
  }

  /** Instance of [[MetaVars]] for [[Inference]]. */
  given [J: MetaVars] => Inference[J] is MetaVars {
    extension (inference: Inference[J]) {
      override def metavariables: Set[MetaVariable] =
        inference.conclusion.metavariables ++ inference.premises.flatMap(_.metavariables)
    }
  }
}
