package proofPlayground
package core.proof.sequent

import scala.language.implicitConversions

import core.meta.Pattern
import core.proof.{Inference, InferenceRule}
import core.proof.sequent.Judgement.*
import core.proof.InferenceDsl.{*, given}

/** Collection of inference rules for sequent calculus. */
object InferenceRules {

  /** Inference rules for classical propositional logic. */
  // noinspection DuplicatedCode
  object ClassicalPropositional {
    import core.logic.propositional.FormulaF
    import core.logic.propositional.FormulaF.*

    /** Axiom (Ax).
      *
      * A ⊢ A.
      */
    val Identity: InferenceRule[Judgement, FormulaF] = {
      val phi = Pattern[FormulaF]("phi")

      Inference(
        "Ax",
        Seq.empty,
        phi |- phi,
      )
    }

    /** Cut.
      *
      * If Γ ⊢ Δ,A and Π,A ⊢ Ω, then Π,Γ ⊢ Ω,Δ.
      */
    val Cut: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val pi    = Pattern[FormulaF]("Pi")
      val omega = Pattern[FormulaF]("Omega")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "Cut",
        Seq(
          gamma |- delta :: phi,
          pi :: phi |- omega,
        ),
        pi :: gamma |- omega :: delta,
      )
    }

    /** Conjunction right introduction (∧R).
      *
      * If Γ ⊢ Δ,A and Γ ⊢ Δ,B, then Γ ⊢ A ∧ B,Δ.
      */
    val ConjunctionRightIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧R",
        Seq(
          gamma |- delta :: phi,
          gamma |- delta :: psi,
        ),
        gamma |- (phi /\ psi) :: delta,
      )
    }

    /** Conjunction left introduction 1 (∧L₁).
      *
      * If Γ,A ⊢ Δ, then Γ,A ∧ B ⊢ Δ.
      */
    val ConjunctionLeftIntroduction1: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧L₁",
        Seq(
          gamma :: phi |- delta,
        ),
        gamma :: (phi /\ psi) |- delta,
      )
    }

    /** Conjunction left introduction 2 (∧L₂).
      *
      * If Γ,B ⊢ Δ, then Γ,A ∧ B ⊢ Δ.
      */
    val ConjunctionLeftIntroduction2: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∧L₂",
        Seq(
          gamma :: psi |- delta,
        ),
        gamma :: (phi /\ psi) |- delta,
      )
    }

    /** Disjunction right introduction 1 (∨R₁).
      *
      * If Γ ⊢ A,Δ, then Γ ⊢ A ∨ B,Δ.
      */
    val DisjunctionRightIntroduction1: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∨R₁",
        Seq(
          gamma |- phi :: delta,
        ),
        gamma |- (phi \/ psi) :: delta,
      )
    }

    /** Disjunction right introduction 2 (∨R₂).
      *
      * If Γ ⊢ B,Δ, then Γ ⊢ A ∨ B,Δ.
      */
    val DisjunctionRightIntroduction2: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∨R₂",
        Seq(
          gamma |- psi :: delta,
        ),
        gamma |- (phi \/ psi) :: delta,
      )
    }

    /** Disjunction left introduction (∨L).
      *
      * If Γ,A ⊢ Δ and Γ,B ⊢ Δ, then Γ,A ∨ B ⊢ Δ.
      */
    val DisjunctionLeftIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∨L",
        Seq(
          gamma :: phi |- delta,
          gamma :: psi |- delta,
        ),
        gamma :: (phi \/ psi) |- delta,
      )
    }

    /** Implication right introduction (→R).
      *
      * If Γ,A ⊢ B,Δ, then Γ ⊢ A → B,Δ.
      */
    val ImplicationRightIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "→R",
        Seq(
          gamma :: phi |- psi :: delta,
        ),
        gamma |- (phi --> psi) :: delta,
      )
    }

    /** Implication left introduction (→L).
      *
      * If Γ ⊢ A,Δ and Π,B ⊢ Ω, then Π,Γ,A → B ⊢ Ω,Δ.
      */
    val ImplicationLeftIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val pi    = Pattern[FormulaF]("Pi")
      val omega = Pattern[FormulaF]("Omega")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "→L",
        Seq(
          gamma |- phi :: delta,
          pi :: psi |- omega,
        ),
        (pi :: gamma) :: (phi --> psi) |- (omega :: delta),
      )
    }

    /** Negation right introduction (¬R).
      *
      * If Γ,A ⊢ Δ, then Γ ⊢ ¬A,Δ.
      */
    val NegationRightIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "¬R",
        Seq(
          gamma :: phi |- delta,
        ),
        gamma |- ~phi :: delta,
      )
    }

    /** Negation left introduction (¬L).
      *
      * If Γ ⊢ A,Δ, then Γ,¬A ⊢ Δ.
      */
    val NegationLeftIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "¬L",
        Seq(
          gamma |- phi :: delta,
        ),
        gamma :: ~phi |- delta,
      )
    }

    /** True right introduction (⊤R).
      *
      * Γ ⊢ ⊤,Δ.
      */
    val TrueRightIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")

      def tru: Pattern[FormulaF] = FormulaF.tru[Pattern[FormulaF]]

      Inference(
        "⊤R",
        Seq.empty,
        gamma |- tru :: delta,
      )
    }

    /** False left introduction (⊥L).
      *
      * Γ,⊥ ⊢ Δ.
      */
    val FalseLeftIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")

      Inference(
        "⊥L",
        Seq.empty,
        gamma :: fls |- delta,
      )
    }

    /** Universal right introduction (∀R).
      *
      * If Γ ⊢ A,Δ, then Γ ⊢ ∀X.A,Δ, where X is not free in Γ and Δ.
      */
    val UniversalRightIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val nu    = Pattern[FormulaF]("nu")

      val sidecondition = Judgement.sidecondition(nu) { [f[_]] => (_) ?=> (quantified, proof) =>
        val judgement = proof.value.judgement
        !(judgement.antecedents ++ judgement.succedents).flatMap(_.freevariables).contains(quantified)
      }

      Inference(
        "∀R",
        Seq(
          gamma |- phi :: delta,
        ),
        gamma |- forall(nu, phi) :: delta,
      )(sidecondition)
    }

    /** Universal left introduction (∀L).
      *
      * If Γ,A[B/X] ⊢ Δ, then Γ,∀X.A ⊢ Δ.
      */
    val UniversalLeftIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")
      val nu    = Pattern[FormulaF]("nu")

      Inference(
        "∀L",
        Seq(
          gamma :: phi(psi / nu) |- delta,
        ),
        gamma :: forall(nu, phi) |- delta,
      )
    }

    /** Existential right introduction (∃R).
      *
      * If Γ ⊢ A[B/X],Δ, then Γ ⊢ ∃X.A,Δ.
      */
    val ExistentialRightIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")
      val nu    = Pattern[FormulaF]("nu")

      Inference(
        "∃R",
        Seq(
          gamma |- phi(psi / nu) :: delta,
        ),
        gamma |- exists(nu, phi) :: delta,
      )
    }

    /** Existential left introduction (∃L).
      *
      * If Γ,A ⊢ Δ, then Γ,∃X.A ⊢ Δ, where X is not free in Γ or Δ.
      */
    val ExistentialLeftIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val delta = Pattern[FormulaF]("Delta")
      val phi   = Pattern[FormulaF]("phi")
      val nu    = Pattern[FormulaF]("nu")

      val sidecondition = Judgement.sidecondition(nu) { [f[_]] => (_) ?=> (quantified, proof) =>
        val judgement = proof.value.judgement
        !(judgement.antecedents ++ judgement.succedents).flatMap(_.freevariables).contains(quantified)
      }

      Inference(
        "∃L",
        Seq(
          gamma :: phi |- delta,
        ),
        gamma :: exists(nu, phi) |- delta,
      )(sidecondition)
    }
  }
}
