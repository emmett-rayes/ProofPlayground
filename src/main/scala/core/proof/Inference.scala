package proofPlayground
package core.proof

import core.Functor
import core.Fix
import core.meta.{FreeVars, MapUnification, MetaVariable, MetaVars, Unify}
import zipper.Tree

/** Representation of a syntactical inference line.
  *
  * @tparam F The type of judgments in this inference.
  * @tparam J The type of judgments in this inference.
  * @param label      The label of the inference line.
  * @param premises   The sequence of premises above the line.
  * @param conclusion The conclusion below the line.
  */
case class Inference[F, J[_]](
  label: String,
  premises: Seq[J[F]],
  conclusion: J[F]
)(val sidecondition: SideCondition[F, J])

object Inference {
  def apply[F, J[_]](label: String, premises: Seq[J[F]], conclusion: J[F]): Inference[F, J] = {
    val trivial = SideCondition[F, J](None, [_[_]] => (_) ?=> (_, _) => true)
    Inference(label, premises, conclusion)(trivial)
  }

  /** [[Functor]] instance for [[Inference]]. */
  given [J[_]: Functor] => Functor[[F] =>> Inference[F, J]] {
    extension [A](inference: Inference[A, J]) {
      override def map[B](f: A => B): Inference[B, J] =
        Inference(
          inference.label,
          inference.premises.map(_.map(f)),
          inference.conclusion.map(f),
        )(inference.sidecondition.map(f))
    }
  }

  /** [[MetaVars]] instance for [[Inference]]. */
  given [F, J[_]] => (J[F] is MetaVars) => Inference[F, J] is MetaVars {
    extension (inference: Inference[F, J]) {
      override def metavariables: Set[MetaVariable] =
        inference.conclusion.metavariables ++ inference.premises.flatMap(_.metavariables)
    }
  }
}
