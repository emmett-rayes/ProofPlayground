package proofPlayground
package core.system.natural

import core.logic.symbol.{Conjunction, Disjunction, Implication}
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
//noinspection DuplicatedCode
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

  /** Conjunction elimination 1 (∧E₁).
   *
   * If Γ ⊢ A ∧ B, then Γ ⊢ A.
   */
  val ConjunctionElimination1: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")

    Inference(
      Set(Judgement(gamma, Pattern.Formula.Concrete(Conjunction(a, b)))),
      Judgement(gamma, a)
    )

  /** Conjunction elimination 2 (∧E₂).
   *
   * If Γ ⊢ A ∧ B, then Γ ⊢ B.
   */
  val ConjunctionElimination2: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")

    Inference(
      Set(Judgement(gamma, Pattern.Formula.Concrete(Conjunction(a, b)))),
      Judgement(gamma, b)
    )

  /** Disjunction introduction 1 (∨I₁).
   *
   * If Γ ⊢ A, then Γ ⊢ A ∨ B.
   */
  val DisjunctionIntroduction1: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")

    Inference(
      Set(Judgement(gamma, a)),
      Judgement(gamma, Pattern.Formula.Concrete(Disjunction(a, b)))
    )

  /** Disjunction introduction 2 (∨I₂).
   *
   * If Γ ⊢ B, then Γ ⊢ A ∨ B.
   */
  val DisjunctionIntroduction2: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")

    Inference(
      Set(Judgement(gamma, b)),
      Judgement(gamma, Pattern.Formula.Concrete(Disjunction(a, b)))
    )

  /** Disjunction elimination (∨E).
   *
   * If Γ ⊢ A ∨ B, Γ,A ⊢ C and Γ,B ⊢ C, then Γ ⊢ C.
   */
  val DisjunctionElimination: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")
    val c = Pattern.Formula.Meta("C")

    Inference(
      Set(
        Judgement(gamma, Pattern.Formula.Concrete(Disjunction(a, b))),
        Judgement(Pattern.Seq.Concrete(scala.Seq(gamma, a)), c),
        Judgement(Pattern.Seq.Concrete(scala.Seq(gamma, b)), c)
      ),
      Judgement(gamma, c)
    )

  /** Implication introduction (→I).
   *
   * If Γ,A ⊢ B, then Γ ⊢ A → B.
   */
  val ImplicationIntroduction: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")

    Inference(
      Set(Judgement(Pattern.Seq.Concrete(scala.Seq(gamma, a)), b)),
      Judgement(gamma, Pattern.Formula.Concrete(Implication(a, b)))
    )

  /** Implication elimination (→E).
   *
   * If Γ ⊢ A → B and Γ ⊢ A, then Γ ⊢ B.
   */
  val ImplicationElimination: InferenceRule =
    val gamma = Pattern.Seq.Meta("Gamma")
    val a = Pattern.Formula.Meta("A")
    val b = Pattern.Formula.Meta("B")

    Inference(
      Set(
        Judgement(gamma, Pattern.Formula.Concrete(Implication(a, b))),
        Judgement(gamma, a)
      ),
      Judgement(gamma, b)
    )
