package proofPlayground
package core.proof

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.{*, given}
import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern
import core.meta.Pattern.given
import core.meta.PatternF.{concrete, meta}
import core.meta.Unifier.given
import core.proof.Assistant
import core.proof.Assistant.ProofResult
import core.proof.natural.Judgement.*
import core.proof.natural.{InferenceRules, Judgement}

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class TestAssistant extends AnyFunSuite {
  import InferenceRules.IntuitionisticPropositional.*

  /** Implicit conversion from a formula to a singleton sequence containing that formula. */
  private given Conversion[Formula, Seq[Formula]] = Seq(_)

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
    private def ::(other: Seq[Pattern[F]]) = other :+ pattern
  }

  test("conjunction introduction for two propositional variables") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A /\ B)
    val rule      = ConjunctionIntroduction

    val result = Assistant.proof(judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A, Seq.empty |- B))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction elimination fails for two propositional variables") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A /\ B)
    val rule      = DisjunctionElimination

    val result = Assistant.proof(judgement, rule)
    assert(!result.isInstanceOf[ProofResult.Success[?, ?]])
  }

  test("disjunction introduction for two propositional variables (left)") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction1

    val result = Assistant.proof(judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction introduction for two propositional variables (right)") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction2

    val result = Assistant.proof(judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- B))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction elimination for two propositional variables with meta-variables in premises only") {
    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = Seq.empty |- C
    val rule      = DisjunctionElimination

    val result = Assistant.proof(judgement, rule, Map(meta("phi") -> A, meta("psi") -> B))
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A \/ B, A |- C, B |- C))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction elimination for a single propositional variable") {
    val phi       = meta("phi"): Pattern[FormulaF]
    val psi       = meta("psi"): Pattern[FormulaF]
    val A         = variable[Formula]("A")
    val judgement = Seq.empty |- A
    val rule      = DisjunctionElimination

    val result = Assistant.proof(judgement, rule)
    result match {
      case ProofResult.SubstitutionFailure(partiallySubstitutedRule) =>
        val expected = Inference(
          rule.label,
          Seq(
            Seq.empty |- phi \/ psi,
            phi |- A.asPattern,
            psi |- A.asPattern,
          ),
          Seq.empty |- A.asPattern,
        )
        assert(partiallySubstitutedRule === expected)
      case _ => fail("Expected substitution error during proof construction")
    }
  }

  test("universal introduction with same variable name in different scopes") {
    val X         = variable[Formula]("X")
    val judgement = Seq(exists[Formula](X, X)) |- forall[Formula](X, X)
    val rule      = UniversalIntroduction

    val result = Assistant.proof(judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        // When forall X.X is matched with forall(nu, phi):
        // nu = X (the binding variable)
        // phi = X (the body, which is the same variable X)
        // The premise should be: X ; exists X.X |- X
        assert(premises.length == 1)
        val premise = premises.head
        assert(premise.free.contains(X))
        assert(premise.assumptions.contains(exists[Formula](X, X)))
        assert(premise.assertion == X)
      case _ => fail("Expected successful proof construction")
    }
  }
}
