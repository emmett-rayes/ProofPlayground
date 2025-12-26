package proofPlayground
package core.system.natural

import core.logic.symbol.Conjunction
import core.system.meta.{Inference, Pattern}

/** An inference rule for natural deduction judgements.
 *
 * Inference rules contain patterns of judgements.
 */
type InferenceRule = Inference[Judgement[Pattern.Seq, Pattern.Formula]]

/** Collection of inference rules for natural deduction.
 *
 * Provides standard logical inference rules used in natural deduction proofs.
 */
case object InferenceRules:

  /** Conjunction introduction (∧I).
   *
   * If Γ ⊢ A and Γ ⊢ B, then Γ ⊢ A ∧ B.
   */
  val ConjunctionIntroduction: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")

    Inference(
      Set(Judgement(gamma, a), Judgement(gamma, b)),
      Judgement(gamma, Pattern.Formula.Concrete(Conjunction(a, b)))
    )
