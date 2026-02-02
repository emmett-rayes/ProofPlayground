package proofPlayground
package core.proof

import core.logic.propositional.Formula
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.meta.PatternF.meta
import core.meta.Substitute.given
import core.meta.Unify.given
import core.meta.MetaVars.given
import core.proof.Assistant
import core.proof.natural.Judgement.*
import core.proof.natural.{InferenceRules, Judgement}

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class TestAssistant extends AnyFunSuite:
  import InferenceRules.IntuitionisticPropositional.*
  import Assistant.ProofResult

  /** Implicit conversion from a formula to a singleton set containing that formula. */
  private given Conversion[Formula, Set[Formula]] = Set(_)

  test("conjunction introduction for two propositional variables") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A /\ B)
    val rule      = ConjunctionIntroduction

    val result = Assistant.proof(judgement, rule)
    result match
      case ProofResult.Success(proof) =>
        val hypotheses = proof.subproofs.map(_.conclusion).toSet
        assert(proof.conclusion == judgement)
        assert(hypotheses == Set(Set.empty |- A, Set.empty |- B))
      case _ => fail("Expected successful proof construction")
  }

  test("disjunction elimination fails for two propositional variables") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A /\ B)
    val rule      = DisjunctionElimination

    val result = Assistant.proof(judgement, rule)
    assert(!result.isInstanceOf[ProofResult.Success[?]])
  }

  test("disjunction introduction for two propositional variables (left)") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction1

    val result = Assistant.proof(judgement, rule)
    result match
      case ProofResult.Success(proof) =>
        val hypotheses = proof.subproofs.map(_.conclusion).toSet
        assert(proof.conclusion == judgement)
        assert(hypotheses == Set(Set.empty |- A))
      case _ => fail("Expected successful proof construction")
  }

  test("disjunction introduction for two propositional variables (right)") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction2

    val result = Assistant.proof(judgement, rule)
    result match
      case ProofResult.Success(proof) =>
        val hypotheses = proof.subproofs.map(_.conclusion).toSet
        assert(proof.conclusion == judgement)
        assert(hypotheses == Set(Set.empty |- B))
      case _ => fail("Expected successful proof construction")
  }

  test("disjunction elimination for two propositional variables with meta-variables in hypotheses only") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = Set.empty |- C
    val rule      = DisjunctionElimination

    val result = Assistant.proof(judgement, rule, Map(meta("phi") -> A, meta("psi") -> B))
    result match
      case ProofResult.Success(proof) =>
        val hypotheses = proof.subproofs.map(_.conclusion).toSet
        assert(proof.conclusion == judgement)
        assert(hypotheses == Set(A |- C, B |- C, Set.empty |- A \/ B))
      case _ => fail("Expected successful proof construction")
  }

  test("disjunction elimination for a single propositional variable") {
    val A         = variable[Formula]("A")
    val judgement = Set.empty |- A
    val rule      = DisjunctionElimination

    val result = Assistant.proof(judgement, rule)
    result match
      case ProofResult.SubstitutionFailure(metavariables) =>
        assert(metavariables === Seq(meta("phi"), meta("psi")))
      case _ => fail("Expected substitution error during proof construction")
  }
