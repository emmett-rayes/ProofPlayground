package proofPlayground
package core.proof

import core.Functor

/** Representation of a syntactical inference line.
  *
  * @tparam J The type of judgments in this inference.
  * @param label      The label of the inference line.
  * @param premises   The sequence of premises above the line.
  * @param conclusion The conclusion below the line.
  */
case class Inference[J](label: String, premises: Seq[J], conclusion: J)

object Inference:
  given Functor[Inference]:
    extension [A](inference: Inference[A])
      override def map[B](f: A => B): Inference[B] =
        Inference(inference.label, inference.premises.map(f), f(inference.conclusion))
