package proofPlayground
package core.system.natural

import core.logic.propositional.FormulaF
import core.logic.propositional.FormulaF.*
import core.meta.PatternF.concrete
import core.meta.{Inference, Pattern, PatternF}
import core.system.natural.Judgement.*

/** An inference rule for natural deduction judgements.
 *
 * Inference rules contain patterns of judgements.
 */
type InferenceRule[F[_]] = Inference[Judgement[Pattern[FormulaF]]]

/** Collection of inference rules for natural deduction. */
case object InferenceRules:

  private given Conversion[FormulaF[Pattern[FormulaF]], Pattern[FormulaF]] = f => Pattern(concrete(f))

  private given Conversion[Pattern[FormulaF], Seq[Pattern[FormulaF]]] = _ :: Nil

  extension (pattern: Pattern[FormulaF])
    private def ::(other: Seq[Pattern[FormulaF]]) = pattern +: other


  /** Inference rules for intuitionistic propositional logic. */
  //noinspection DuplicatedCode
  case object IntuitionisticPropositional:

    /** Conjunction introduction (∧I).
     *
     * If Γ ⊢ A and Γ ⊢ B, then Γ ⊢ A ∧ B.
     */
    val ConjunctionIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")

      Inference(
        Set(
          gamma |- phi,
          gamma |- psi,
        ),
        gamma |- phi /\ psi,
      )

    /** Conjunction elimination 1 (∧E₁).
     *
     * If Γ ⊢ A ∧ B, then Γ ⊢ A.
     */
    val ConjunctionElimination1: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")

      Inference(
        Set(
          gamma |- phi /\ psi,
        ),
        gamma |- phi,
      )

    /** Conjunction elimination 2 (∧E₂).
     *
     * If Γ ⊢ A ∧ B, then Γ ⊢ B.
     */
    val ConjunctionElimination2: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")

      Inference(
        Set(
          gamma |- phi /\ psi,
        ),
        gamma |- psi,
      )

    /** Disjunction introduction 1 (∨I₁).
     *
     * If Γ ⊢ A, then Γ ⊢ A ∨ B.
     */
    val DisjunctionIntroduction1: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")

      Inference(
        Set(
          gamma |- phi,
        ),
        gamma |- phi \/ psi,
      )

    /** Disjunction introduction 2 (∨I₂).
     *
     * If Γ ⊢ B, then Γ ⊢ A ∨ B.
     */
    val DisjunctionIntroduction2: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")

      Inference(
        Set(
          gamma |- psi,
        ),
        gamma |- phi \/ psi,
      )

    /** Disjunction elimination (∨E).
     *
     * If Γ ⊢ A ∨ B, Γ,A ⊢ C and Γ,B ⊢ C, then Γ ⊢ C.
     */
    val DisjunctionElimination: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")
      val rho = Pattern[FormulaF]("rho")

      Inference(
        Set(
          gamma |- phi \/ psi,
          gamma :: phi |- rho,
          gamma :: psi |- rho,
        ),
        gamma |- rho,
      )

    /** Implication introduction (→I).
     *
     * If Γ,A ⊢ B, then Γ ⊢ A → B.
     */
    val ImplicationIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")

      Inference(
        Set(
          gamma :: phi |- psi,
        ),
        gamma |- phi --> psi,
      )

    /** Implication elimination (→E).
     *
     * If Γ ⊢ A → B and Γ ⊢ A, then Γ ⊢ B.
     */
    val ImplicationElimination: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")
      val psi = Pattern[FormulaF]("psi")

      Inference(
        Set(
          gamma |- phi --> psi,
          gamma |- phi,
        ),
        gamma |- psi,
      )

    /** Negation introduction (¬I).
     *
     * If Γ,A ⊢ ⊥, then Γ ⊢ ¬A.
     */
    val NegationIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")

      Inference(
        Set(
          gamma :: phi |- fls,
        ),
        gamma |- ~phi,
      )

    /** Negation elimination (¬E).
     *
     * If Γ ⊢ ¬A and Γ ⊢ A, then Γ ⊢ ⊥.
     */
    val NegationElimination: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")

      Inference(
        Set(
          gamma |- ~phi,
          gamma |- phi,
        ),
        gamma |- fls,
      )

    /** True introduction (⊤I).
     *
     * Principle of implosion (ex quodlibet verum).
     *
     * Γ ⊢ ⊤.
     */
    val TrueIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")

      Inference(
        Set(),
        gamma |- tru,
      )

    /** False elimination (⊥E).
     *
     * Principle of explosion (ex falso quodlibet).
     *
     * If Γ ⊢ ⊥, then Γ ⊢ A.
     */
    val FalseElimination: InferenceRule[FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi = Pattern[FormulaF]("phi")

      Inference(
        Set(
          gamma |- fls
        ),
        gamma |- phi,
      )
