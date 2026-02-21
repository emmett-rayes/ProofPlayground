package proofPlayground
package core.proof.natural

import scala.language.implicitConversions

import core.meta.PatternF.{concrete, substitution}
import core.meta.{MetaVariable, Pattern, PatternF}
import core.meta.Pattern.given
import core.proof.natural.Judgement.*
import core.proof.{Inference, InferenceRule}

/** Collection of inference rules for natural deduction. */
case object InferenceRules {
  opaque type SubstitutionContext[F[_]] = (Pattern[F], Pattern[F])

  /** Implicit conversion from a formula over patterns to a pattern. */
  private given [F[_]] => Conversion[F[Pattern[F]], Pattern[F]] = f => Pattern(concrete(f))

  /** Implicit conversion from a pattern to a singleton sequence containing that pattern. */
  private given [F[_]] => Conversion[Pattern[F], Seq[Pattern[F]]] = Seq(_)

  extension [F[_]](pattern: Pattern[F]) {

    /** Add a pattern to a sequence of patterns.
      *
      * @param other The sequence of patterns to add to.
      * @return A new sequence containing the original patterns and the added pattern.
      */
    private def ::(other: Seq[Pattern[F]]): Seq[Pattern[F]] = other :+ pattern

    private def /(other: Pattern[F]): SubstitutionContext[F] = (pattern, other)

    private def apply(context: SubstitutionContext[F]): Pattern[F] =
      substitution(context._2, context._1, pattern)
  }

  /** Inference rules for intuitionistic propositional logic. */
  // noinspection DuplicatedCode
  case object IntuitionisticPropositional {
    import core.logic.propositional.FormulaF
    import core.logic.propositional.FormulaF.*

    /** Conjunction introduction (∧I).
      *
      * If Γ ⊢ A and Γ ⊢ B, then Γ ⊢ A ∧ B.
      */
    val ConjunctionIntroduction: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧I",
        Seq(
          omega % gamma |- phi,
          omega % gamma |- psi,
        ),
        omega % gamma |- phi /\ psi,
      )
    }

    /** Conjunction elimination 1 (∧E₁).
      *
      * If Γ ⊢ A ∧ B, then Γ ⊢ A.
      */
    val ConjunctionElimination1: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧E₁",
        Seq(
          omega % gamma |- phi /\ psi,
        ),
        omega % gamma |- phi,
      )
    }

    /** Conjunction elimination 2 (∧E₂).
      *
      * If Γ ⊢ A ∧ B, then Γ ⊢ B.
      */
    val ConjunctionElimination2: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧E₂",
        Seq(
          omega % gamma |- phi /\ psi,
        ),
        omega % gamma |- psi,
      )
    }

    /** Disjunction introduction 1 (∨I₁).
      *
      * If Γ ⊢ A, then Γ ⊢ A ∨ B.
      */
    val DisjunctionIntroduction1: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∨I₁",
        Seq(
          omega % gamma |- phi,
        ),
        omega % gamma |- phi \/ psi,
      )
    }

    /** Disjunction introduction 2 (∨I₂).
      *
      * If Γ ⊢ B, then Γ ⊢ A ∨ B.
      */
    val DisjunctionIntroduction2: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∨I₂",
        Seq(
          omega % gamma |- psi,
        ),
        omega % gamma |- phi \/ psi,
      )
    }

    /** Disjunction elimination (∨E).
      *
      * If Γ ⊢ A ∨ B, Γ,A ⊢ C and Γ,B ⊢ C, then Γ ⊢ C.
      */
    val DisjunctionElimination: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")
      val rho   = Pattern[FormulaF]("rho")

      Inference(
        "∨E",
        Seq(
          omega % gamma |- phi \/ psi,
          omega % (gamma :: phi) |- rho,
          omega % (gamma :: psi) |- rho,
        ),
        omega % gamma |- rho,
      )
    }

    /** Implication introduction (→I).
      *
      * If Γ,A ⊢ B, then Γ ⊢ A → B.
      */
    val ImplicationIntroduction: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "→I",
        Seq(
          omega % (gamma :: phi) |- psi,
        ),
        omega % gamma |- phi --> psi,
      )
    }

    /** Implication elimination (→E).
      *
      * If Γ ⊢ A → B and Γ ⊢ A, then Γ ⊢ B.
      */
    val ImplicationElimination: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "→E",
        Seq(
          omega % gamma |- phi --> psi,
          omega % gamma |- phi,
        ),
        omega % gamma |- psi,
      )
    }

    /** Negation introduction (¬I).
      *
      * If Γ,A ⊢ ⊥, then Γ ⊢ ¬A.
      */
    val NegationIntroduction: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "¬I",
        Seq(
          omega % (gamma :: phi) |- fls,
        ),
        omega % gamma |- ~phi,
      )
    }

    /** Negation elimination (¬E).
      *
      * If Γ ⊢ ¬A and Γ ⊢ A, then Γ ⊢ ⊥.
      */
    val NegationElimination: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "¬E",
        Seq(
          omega % gamma |- ~phi,
          omega % gamma |- phi,
        ),
        omega % gamma |- fls,
      )
    }

    /** True introduction (⊤I).
      *
      * Principle of implosion (ex quodlibet verum).
      *
      * Γ ⊢ ⊤.
      */
    val TrueIntroduction: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")

      Inference(
        "⊤I",
        Seq(),
        omega % gamma |- tru,
      )
    }

    /** False elimination (⊥E).
      *
      * Principle of explosion (ex falso quodlibet).
      *
      * If Γ ⊢ ⊥, then Γ ⊢ A.
      */
    val FalseElimination: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "⊥E",
        Seq(
          omega % gamma |- fls
        ),
        omega % gamma |- phi,
      )
    }

    /** Universal introduction (∀I).
      */
    val UniversalIntroduction: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "∀I",
        Seq(
          (omega :: nu) % gamma |- phi
        ),
        omega % gamma |- forall(nu, phi)
      )
    }

    /** Universal elimination (∀E).
      */
    val UniversalElimination: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∀E",
        Seq(
          omega % gamma |- forall(nu, phi)
        ),
        omega % gamma |- phi(psi / nu)
      )
    }

    /** Existential introduction (∃I).
      */
    val ExistentialIntroduction: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∃I",
        Seq(
          omega % gamma |- phi(psi / nu)
        ),
        omega % gamma |- exists(nu, phi)
      )
    }

    /** Existential elimination (∃E).
      */
    val ExistentialElimination: InferenceRule[Judgement, FormulaF] = {
      val omega = Pattern[FormulaF]("Omega")
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val rho   = Pattern[FormulaF]("rho")

      Inference(
        "∃E",
        Seq(
          omega         % gamma |- exists(nu, phi),
          (omega :: nu) % (gamma :: phi) |- rho,
        ),
        omega % gamma |- rho,
      )
    }
  }
}
