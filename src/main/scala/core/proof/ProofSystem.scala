package proofPlayground
package core.proof

import core.logic.propositional
import core.meta.Pattern
import core.proof.ObjectSeq.objectSeq

/** An inference rule used in a proof system.
  *
  * Inference rules contain judgements over patterns, i.e. judgement schemas.
  *
  * @tparam J The judgement type used in the proof system.
  * @tparam F The formula functor used in the judgements.
  */
type InferenceRule[J[_], F[_]] = Inference[J[Pattern[F]]]

/** Representation of a proof system.
  *
  * A proof system consists of a set of inference rules specific to a given logic and judgement form.
  *
  * @tparam J The judgement type used in the proof system.
  * @tparam F The formula functor used in the judgements.
  */
case class ProofSystem[J[_], F[_]](rules: Set[InferenceRule[J, F]])

object ProofSystem {
  /** Proof system for intuitionistic propositional natural deduction. */
  val IntuitionisticPropositionalNaturalDeduction: ProofSystem[natural.Judgement, propositional.FormulaF] = {
    import natural.InferenceRules.IntuitionisticPropositional
    ProofSystem(objectSeq(IntuitionisticPropositional).toSet)
  }
}
