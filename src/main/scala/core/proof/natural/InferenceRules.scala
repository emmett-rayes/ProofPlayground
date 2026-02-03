package proofPlayground
package core.proof.natural

import core.meta.PatternF.concrete
import core.meta.{Pattern, PatternF}
import core.proof.{Inference, InferenceRule}
import core.proof.natural.Judgement.*

import scala.language.implicitConversions

/** Collection of inference rules for natural deduction. */
case object InferenceRules:
  /** Implicit conversion from a formula over patterns to a pattern. */
  private given [F[_]] => Conversion[F[Pattern[F]], Pattern[F]] = f => Pattern(concrete(f))

  /** Implicit conversion from a pattern to a singleton sequence containing that pattern. */
  private given [F[_]] => Conversion[Pattern[F], Seq[Pattern[F]]] = Seq(_)

  extension [F[_]](pattern: Pattern[F])
    /** Add a pattern to a sequence of patterns.
      *
      * @param other The sequence of patterns to add to.
      * @return A new sequence containing the original patterns and the added pattern.
      */
    private def ::(other: Seq[Pattern[F]]) = other :+ pattern

  /** Inference rules for intuitionistic propositional logic. */
  // noinspection DuplicatedCode
  case object IntuitionisticPropositional:
    import core.logic.propositional.FormulaF
    import core.logic.propositional.FormulaF.*

    /** Conjunction introduction (∧I).
      *
      * If Γ ⊢ A and Γ ⊢ B, then Γ ⊢ A ∧ B.
      */
    val ConjunctionIntroduction: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧I",
        Seq(
          gamma |- phi,
          gamma |- psi,
        ),
        gamma |- phi /\ psi,
      )

    /** Conjunction elimination 1 (∧E₁).
      *
      * If Γ ⊢ A ∧ B, then Γ ⊢ A.
      */
    val ConjunctionElimination1: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧E₁",
        Seq(
          gamma |- phi /\ psi,
        ),
        gamma |- phi,
      )

    /** Conjunction elimination 2 (∧E₂).
      *
      * If Γ ⊢ A ∧ B, then Γ ⊢ B.
      */
    val ConjunctionElimination2: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧E₂",
        Seq(
          gamma |- phi /\ psi,
        ),
        gamma |- psi,
      )

    /** Disjunction introduction 1 (∨I₁).
      *
      * If Γ ⊢ A, then Γ ⊢ A ∨ B.
      */
    val DisjunctionIntroduction1: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∨I₁",
        Seq(
          gamma |- phi,
        ),
        gamma |- phi \/ psi,
      )

    /** Disjunction introduction 2 (∨I₂).
      *
      * If Γ ⊢ B, then Γ ⊢ A ∨ B.
      */
    val DisjunctionIntroduction2: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∨I₂",
        Seq(
          gamma |- psi,
        ),
        gamma |- phi \/ psi,
      )

    /** Disjunction elimination (∨E).
      *
      * If Γ ⊢ A ∨ B, Γ,A ⊢ C and Γ,B ⊢ C, then Γ ⊢ C.
      */
    val DisjunctionElimination: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")
      val rho   = Pattern[FormulaF]("rho")

      Inference(
        "∨E",
        Seq(
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
    val ImplicationIntroduction: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "→I",
        Seq(
          gamma :: phi |- psi,
        ),
        gamma |- phi --> psi,
      )

    /** Implication elimination (→E).
      *
      * If Γ ⊢ A → B and Γ ⊢ A, then Γ ⊢ B.
      */
    val ImplicationElimination: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "→E",
        Seq(
          gamma |- phi --> psi,
          gamma |- phi,
        ),
        gamma |- psi,
      )

    /** Negation introduction (¬I).
      *
      * If Γ,A ⊢ ⊥, then Γ ⊢ ¬A.
      */
    val NegationIntroduction: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "¬I",
        Seq(
          gamma :: phi |- fls,
        ),
        gamma |- ~phi,
      )

    /** Negation elimination (¬E).
      *
      * If Γ ⊢ ¬A and Γ ⊢ A, then Γ ⊢ ⊥.
      */
    val NegationElimination: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "¬E",
        Seq(
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
    val TrueIntroduction: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")

      Inference(
        "⊤I",
        Seq(),
        gamma |- tru,
      )

    /** False elimination (⊥E).
      *
      * Principle of explosion (ex falso quodlibet).
      *
      * If Γ ⊢ ⊥, then Γ ⊢ A.
      */
    val FalseElimination: InferenceRule[Judgement, FormulaF] =
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "⊥E",
        Seq(
          gamma |- fls
        ),
        gamma |- phi,
      )
