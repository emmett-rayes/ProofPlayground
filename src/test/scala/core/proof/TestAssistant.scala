package proofPlayground
package core.proof

import core.logic.propositional.Formula
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.meta.PatternF.meta
import core.proof.Assistant
import core.proof.natural.Judgement.*
import core.proof.natural.{InferenceRules, Judgement}

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class TestAssistant extends AnyFunSuite:
  import InferenceRules.IntuitionisticPropositional.*

  /** Implicit conversion from a formula to a singleton set containing that formula. */
  private given Conversion[Formula, Set[Formula]] = Set(_)

  test("conjunction introduction for two propositional variables") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A /\ B)
    val rule      = ConjunctionIntroduction

    val proof = Assistant.proof(judgement, rule)
    assert(proof.isDefined)

    val hypotheses = proof.get.subproofs.map(_.conclusion).toSet
    assert(proof.get.conclusion == judgement)
    assert(hypotheses == Set(Set.empty |- A, Set.empty |- B))
  }

  test("disjunction elimination fails for two propositional variables") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A /\ B)
    val rule      = DisjunctionElimination

    val proof = Assistant.proof(judgement, rule)
    assert(proof.isEmpty)
  }

  test("disjunction introduction for two propositional variables (left)") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction1

    val proof = Assistant.proof(judgement, rule)
    assert(proof.isDefined)

    val hypotheses = proof.get.subproofs.map(_.conclusion).toSet
    assert(proof.get.conclusion == judgement)
    assert(hypotheses == Set(Set.empty |- A))
  }

  test("disjunction introduction for two propositional variables (right)") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Set.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction2

    val proof = Assistant.proof(judgement, rule)
    assert(proof.isDefined)

    val hypotheses = proof.get.subproofs.map(_.conclusion).toSet
    assert(proof.get.conclusion == judgement)
    assert(hypotheses == Set(Set.empty |- B))
  }

  test("disjunction elimination for two propositional variables with meta-variables in hypotheses only") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = Set.empty |- C
    val rule      = DisjunctionElimination

    val proof = Assistant.proof(judgement, rule, Map(meta("phi") -> A, meta("psi") -> B))
    assert(proof.isDefined)

    val hypotheses = proof.get.subproofs.map(_.conclusion).toSet
    assert(proof.get.conclusion == judgement)
    assert(hypotheses == Set(A |- C, B |- C, Set.empty |- A \/ B))
  }
