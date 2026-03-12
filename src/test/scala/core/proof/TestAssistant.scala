package proofPlayground
package core.proof

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.{*, given}
import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern
import core.meta.Pattern.given
import core.meta.PatternF.{concrete, meta}
import core.meta.Unification
import core.meta.Unify.given
import core.proof.Assistant
import core.proof.Assistant.ProofResult
import core.proof.Proof.*
import core.proof.ProofRequirements.given
import core.proof.natural.Judgement
import core.proof.sequent.Judgement

class TestAssistant extends AnyFunSuite {
  import core.proof.natural.InferenceRules.IntuitionisticPropositional.*
  import core.proof.sequent.InferenceRules.ClassicalPropositional.*

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
    import core.proof.natural.Judgement.*
    import core.proof.natural.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A /\ B)
    val rule      = ConjunctionIntroduction

    val result = Assistant.proof[FormulaF, natural.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A, Seq.empty |- B))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction elimination fails for two propositional variables") {
    import core.proof.natural.Judgement.*
    import core.proof.natural.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A /\ B)
    val rule      = DisjunctionElimination

    val result = Assistant.proof[FormulaF, natural.Judgement](judgement, rule)
    assert(!result.isInstanceOf[ProofResult.Success[?, ?]])
  }

  test("disjunction introduction for two propositional variables (left)") {
    import core.proof.natural.Judgement.*
    import core.proof.natural.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction1

    val result = Assistant.proof[FormulaF, natural.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction introduction for two propositional variables (right)") {
    import core.proof.natural.Judgement.*
    import core.proof.natural.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A \/ B)
    val rule      = DisjunctionIntroduction2

    val result = Assistant.proof[FormulaF, natural.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- B))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction elimination for two propositional variables with meta-variables in premises only") {
    import core.proof.natural.Judgement.*
    import core.proof.natural.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = Seq.empty |- C
    val rule      = DisjunctionElimination

    given req: ProofRequirements[FormulaF, natural.Judgement] = summon
    val unification = req.Uni.empty[Formula].update(Map(meta("phi") -> A, meta("psi") -> B)).get

    val result = Assistant.proof[FormulaF, natural.Judgement](judgement, rule, unification)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A \/ B, A |- C, B |- C))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("disjunction elimination for a single propositional variable") {
    import core.proof.natural.Judgement.*
    import core.proof.natural.Judgement.given

    val phi       = meta("phi"): Pattern[FormulaF]
    val psi       = meta("psi"): Pattern[FormulaF]
    val A         = variable[Formula]("A")
    val judgement = Seq.empty |- A
    val rule      = DisjunctionElimination

    val result = Assistant.proof[FormulaF, natural.Judgement](judgement, rule)
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
    import core.proof.natural.Judgement.*
    import core.proof.natural.Judgement.given

    val X         = variable[Formula]("X")
    val judgement = Seq(exists[Formula](X, X)) |- forall[Formula](X, X)
    val rule      = UniversalIntroduction

    val result = Assistant.proof[FormulaF, natural.Judgement](judgement, rule)
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
        assert(premise.assumptions.contains(exists[Formula](X, X)))
        assert(premise.assertion == X)
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus implication right introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A --> B)
    val rule      = ImplicationRightIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(A |- B))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus identity axiom") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val judgement = A |- A
    val rule      = Identity

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq.empty)
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus conjunction right introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A /\ B)
    val rule      = ConjunctionRightIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A, Seq.empty |- B))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus conjunction left introduction 1") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = (A /\ B) |- C
    val rule      = ConjunctionLeftIntroduction1

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(A |- C))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus conjunction left introduction 2") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = (A /\ B) |- C
    val rule      = ConjunctionLeftIntroduction2

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(B |- C))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus disjunction right introduction 1") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A \/ B)
    val rule      = DisjunctionRightIntroduction1

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- A))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus disjunction right introduction 2") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = Seq.empty |- (A \/ B)
    val rule      = DisjunctionRightIntroduction2

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- B))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus disjunction left introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = (A \/ B) |- C
    val rule      = DisjunctionLeftIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(A |- C, B |- C))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus implication left introduction with meta-variables") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val C         = variable[Formula]("C")
    val judgement = (A --> B) |- C
    val rule      = ImplicationLeftIntroduction

    // This rule has multiple context variables that need to be specified
    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.SubstitutionFailure(_) =>
        // Expected: the Assistant can't determine how to split contexts without additional hints
        fail("TODO")
      case _ => fail("Expected substitution failure for this complex rule")
    }
  }

  test("sequent calculus negation right introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val judgement = Seq.empty |- ~A
    val rule      = NegationRightIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(A |- Seq.empty))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus negation left introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = ~A |- B
    val rule      = NegationLeftIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq(Seq.empty |- Seq(B, A)))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus true right introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val judgement = Seq.empty |- tru[Formula]
    val rule      = TrueRightIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq.empty)
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus false left introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val judgement = fls[Formula] |- A
    val rule      = FalseLeftIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises == Seq.empty)
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus cut rule with meta-variables") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val A         = variable[Formula]("A")
    val B         = variable[Formula]("B")
    val judgement = A |- B
    val rule      = Cut

    // This rule has multiple context variables that need to be specified
    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.SubstitutionFailure(_) =>
        // Expected: the Assistant can't determine how to split contexts without additional hints
        fail("TODO")
      case _ => fail("Expected substitution failure for this complex rule")
    }
  }

  test("sequent calculus universal right introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val X         = variable[Formula]("X")
    val judgement = Seq.empty |- forall[Formula](X, X)
    val rule      = UniversalRightIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises.length == 1)
        val premise = premises.head
        assert(premise.antecedents == Seq.empty)
        assert(premise.succedents.contains(X))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus existential left introduction") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val X         = variable[Formula]("X")
    val A         = variable[Formula]("A")
    val judgement = exists[Formula](X, X) |- A
    val rule      = ExistentialLeftIntroduction

    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.Success(proof) =>
        val premises = proof.subproofs.map(_.conclusion)
        assert(proof.conclusion == judgement)
        assert(premises.length == 1)
        val premise = premises.head
        assert(premise.antecedents.contains(X))
        assert(premise.succedents.contains(A))
      case _ => fail("Expected successful proof construction")
    }
  }

  test("sequent calculus existential right introduction with meta-variables") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val X         = variable[Formula]("X")
    val judgement = Seq.empty |- exists[Formula](X, X)
    val rule      = ExistentialRightIntroduction

    // This rule involves pattern substitution which is complex
    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.SubstitutionFailure(_) =>
        // Expected: the Assistant can't fully determine the substitution pattern
        fail("TODO")
      case _ => fail("Expected substitution failure for this rule with pattern substitution")
    }
  }

  test("sequent calculus universal left introduction with meta-variables") {
    import core.proof.sequent.Judgement.*
    import core.proof.sequent.Judgement.given

    val X         = variable[Formula]("X")
    val B         = variable[Formula]("B")
    val judgement = forall[Formula](X, X) |- B
    val rule      = UniversalLeftIntroduction

    // This rule involves pattern substitution which is complex
    val result = Assistant.proof[FormulaF, sequent.Judgement](judgement, rule)
    result match {
      case ProofResult.SubstitutionFailure(_) =>
        // Expected: the Assistant can't fully determine the substitution pattern
        fail("TODO")
      case _ => fail("Expected substitution failure for this rule with pattern substitution")
    }
  }
}
