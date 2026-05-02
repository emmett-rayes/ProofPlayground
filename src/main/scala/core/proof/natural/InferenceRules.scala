package proofPlayground
package core.proof.natural

import scala.language.implicitConversions

import core.Fix
import core.meta.{MetaVariable, Pattern}
import core.proof.natural.Judgement.*
import core.proof.{Inference, InferenceRule, SideCondition}
import core.proof.InferenceDsl.{*, given}
import core.proof.Proof
import core.proof.Proof.asTree
import zipper.Tree

extension [F[_]](proof: Proof[F, Judgement]) {
  // judgement assumptions are accumulative, so we only consider assumptions that were added after the current judgement.
  def strippedChildren: List[Proof[F, Judgement]] =
    proof.asTree.children.map { child =>
      val carried    = proof.value.judgement.assumptions
      val introduced = child.value.judgement.assumptions.diff(proof.value.judgement.assumptions)
      val stripped   = child.map { node =>
        node.map { judgement =>
          judgement.copy(assumptions = judgement.assumptions.diff(carried).diff(introduced))
        }
      }
      proof.copy(value = stripped.value, children = stripped.strippedChildren)
    }
}

/** Collection of inference rules for natural deduction. */
case object InferenceRules {

  /** Inference rules for intuitionistic propositional logic. */
  // noinspection DuplicatedCode
  object IntuitionisticPropositional {
    import core.logic.propositional.FormulaF
    import core.logic.propositional.FormulaF.*

    /** Conjunction introduction (∧I).
      *
      * If Γ ⊢ A and Γ ⊢ B, then Γ ⊢ A ∧ B.
      */
    val ConjunctionIntroduction: InferenceRule[Judgement, FormulaF] = {
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
    }

    /** Conjunction elimination 1 (∧E₁).
      *
      * If Γ ⊢ A ∧ B, then Γ ⊢ A.
      */
    val ConjunctionElimination1: InferenceRule[Judgement, FormulaF] = {
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
    }

    /** Conjunction elimination 2 (∧E₂).
      *
      * If Γ ⊢ A ∧ B, then Γ ⊢ B.
      */
    val ConjunctionElimination2: InferenceRule[Judgement, FormulaF] = {
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
    }

    /** Disjunction introduction 1 (∨I₁).
      *
      * If Γ ⊢ A, then Γ ⊢ A ∨ B.
      */
    val DisjunctionIntroduction1: InferenceRule[Judgement, FormulaF] = {
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
    }

    /** Disjunction introduction 2 (∨I₂).
      *
      * If Γ ⊢ B, then Γ ⊢ A ∨ B.
      */
    val DisjunctionIntroduction2: InferenceRule[Judgement, FormulaF] = {
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
    }

    /** Disjunction elimination (∨E).
      *
      * If Γ ⊢ A ∨ B, Γ,A ⊢ C and Γ,B ⊢ C, then Γ ⊢ C.
      */
    val DisjunctionElimination: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")
      val rho   = Pattern[FormulaF]("rho")

      Inference(
        "∨E",
        Seq(
          gamma |- phi \/ psi,
          (gamma :: phi) |- rho,
          (gamma :: psi) |- rho,
        ),
        gamma |- rho,
      )
    }

    /** Implication introduction (→I).
      *
      * If Γ,A ⊢ B, then Γ ⊢ A → B.
      */
    val ImplicationIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "→I",
        Seq(
          (gamma :: phi) |- psi,
        ),
        gamma |- phi --> psi,
      )
    }

    /** Implication elimination (→E).
      *
      * If Γ ⊢ A → B and Γ ⊢ A, then Γ ⊢ B.
      */
    val ImplicationElimination: InferenceRule[Judgement, FormulaF] = {
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
    }

    /** Negation introduction (¬I).
      *
      * If Γ,A ⊢ ⊥, then Γ ⊢ ¬A.
      */
    val NegationIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "¬I",
        Seq(
          (gamma :: phi) |- fls,
        ),
        gamma |- ~phi,
      )
    }

    /** Negation elimination (¬E).
      *
      * If Γ ⊢ ¬A and Γ ⊢ A, then Γ ⊢ ⊥.
      */
    val NegationElimination: InferenceRule[Judgement, FormulaF] = {
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
    }

    /** True introduction (⊤I).
      *
      * Principle of implosion (ex quodlibet verum).
      *
      * Γ ⊢ ⊤.
      */
    val TrueIntroduction: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")

      Inference(
        "⊤I",
        Seq.empty,
        gamma |- tru,
      )
    }

    /** False elimination (⊥E).
      *
      * Principle of explosion (ex falso quodlibet).
      *
      * If Γ ⊢ ⊥, then Γ ⊢ A.
      */
    val FalseElimination: InferenceRule[Judgement, FormulaF] = {
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      Inference(
        "⊥E",
        Seq(
          gamma |- fls,
        ),
        gamma |- phi,
      )
    }

    /** Universal introduction (∀I).
      *
      * If Γ ⊢ A, then Γ ⊢ ∀X.A, where X is not free in Γ.
      */
    val UniversalIntroduction: InferenceRule[Judgement, FormulaF] = {
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")

      // hack: using judgement as a pattern container (stored in assertion) to allow easy substitution
      val sidecondition =
        Judgement.sidecondition(nu) { [f[_]] => (_) ?=> (quantified, proof) =>
          proof.strippedChildren.head.leaves.map(_.value.judgement).forall { judgement =>
            val closed = judgement.assumptions.contains(judgement.assertion)
            if closed then true
            else !judgement.assertion.freevariables.contains(quantified)
          }
        }

      Inference(
        "∀I",
        Seq(
          gamma |- phi,
        ),
        gamma |- forall(nu, phi),
      )(sidecondition)
    }

    /** Universal elimination (∀E).
      *
      * If Γ ⊢ ∀X.A, then Γ ⊢ A[B/X].
      */
    val UniversalElimination: InferenceRule[Judgement, FormulaF] = {
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∀E",
        Seq(
          gamma |- forall(nu, phi),
        ),
        gamma |- phi(psi / nu),
      )
    }

    /** Existential introduction (∃I).
      *
      * If Γ ⊢ A[B/X], then Γ ⊢ ∃X.A.
      */
    val ExistentialIntroduction: InferenceRule[Judgement, FormulaF] = {
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val psi   = Pattern[FormulaF]("psi")

      Inference(
        "∃I",
        Seq(
          gamma |- phi(psi / nu),
        ),
        gamma |- exists(nu, phi),
      )
    }

    /** Existential elimination (∃E).
      * If Γ ⊢ ∃X.A and Γ,A ⊢ B, then Γ ⊢ B, where X is not free in Γ or B.
      */
    val ExistentialElimination: InferenceRule[Judgement, FormulaF] = {
      val nu    = Pattern[FormulaF]("nu")
      val gamma = Pattern[FormulaF]("Gamma")
      val phi   = Pattern[FormulaF]("phi")
      val rho   = Pattern[FormulaF]("rho")

      // hack: using judgement as a pattern container (stored in assertion) to allow easy substitution
      val sidecondition =
        Judgement.sidecondition(nu) { [f[_]] => (_) ?=> (quantified, proof) =>
          !proof.strippedChildren.head.value.judgement.assertion.freevariables.contains(quantified) &&
          proof.strippedChildren.head.leaves.map(_.value.judgement).forall { judgement =>
            val closed = judgement.assumptions.contains(judgement.assertion)
            if closed then true
            else !judgement.assertion.freevariables.contains(quantified)
          }
        }

      Inference(
        "∃E",
        Seq(
          gamma |- exists(nu, phi),
          gamma :: phi |- rho,
        ),
        (gamma |- rho),
      )(sidecondition)
    }
  }
}
