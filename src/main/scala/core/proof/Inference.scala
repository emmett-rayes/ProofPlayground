package proofPlayground
package core.proof

import core.Functor
import core.meta.{MetaVariable, MetaVars}

/** Representation of a syntactical inference line.
  *
  * @tparam J The type of judgments in this inference.
  * @param label      The label of the inference line.
  * @param premises   The sequence of premises above the line.
  * @param conclusion The conclusion below the line.
  */
case class Inference[J](label: String, premises: Seq[J], conclusion: J)

object Inference {

  /** [[Functor]] instance for [[Inference]]. */
  given Functor[Inference] {
    extension [A](inference: Inference[A]) {
      override def map[B](f: A => B): Inference[B] =
        Inference(inference.label, inference.premises.map(f), f(inference.conclusion))
    }
  }

  /** [[MetaVars]] instance for [[Inference]]. */
  given [J: MetaVars] => Inference[J] is MetaVars {
    extension (inference: Inference[J]) {
      override def metavariables: Set[MetaVariable] =
        inference.conclusion.metavariables ++ inference.premises.flatMap(_.metavariables)
    }
  }
}
