package proofPlayground
package core.proof

import core.logic.propositional.Formula
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.proof.Assistant
import core.proof.natural.Judgement.*
import core.proof.natural.{InferenceRules, Judgement}

import org.scalatest.funsuite.AnyFunSuite

class TestAssistant extends AnyFunSuite:
  import InferenceRules.IntuitionisticPropositional.*

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
