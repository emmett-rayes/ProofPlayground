package proofPlayground
package core.meta

import core.proof.Inference

/** A typeclass for extracting meta-variables from a container. */
trait MetaVars {
  type Self

  extension (self: Self) {

    /** Returns the set of meta-variables appearing in `self`. */
    def metavariables: Set[MetaVariable]
  }
}

object MetaVars {


  /** [[MetaVars]] instance for [[Inference]]. */
  given [J: MetaVars] => Inference[J] is MetaVars {
    extension (inference: Inference[J]) {
      override def metavariables: Set[MetaVariable] =
        inference.conclusion.metavariables ++ inference.premises.flatMap(_.metavariables)
    }
  }
}
