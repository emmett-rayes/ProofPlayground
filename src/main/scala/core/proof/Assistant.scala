package proofPlayground
package core.proof

import core.Traverse.traverse
import core.meta.*
import core.meta.Substitute.substitute
import core.meta.Unify.unify
import core.proof.natural.Judgement

object Assistant:
  /** Attempts to produce a proof for the given judgement by applying the given inference rule.
    *
    * The proof is constructible if the judgement can be proved by applying the inference rule to the judgement.
    * Unification is used to match the patterns in the inference rule with the concrete formulas in the judgement.
    * Substitution is used to substitute the unification results into the patterns in the inference rule.
    * Thus producing new hypotheses as given by the inference rule.
    *
    * @tparam F The type of the formula functor used in patterns in the inference rule.
    * @tparam T The type of the concrete formula used in the judgement.
    * @param judgement The judgement to be proved.
    * @param rule The inference rule to be applied.
    * @return Some(proof) if it is constructible, None otherwise.
    */
  def proof[F[_], T: {Unify { type Functor = F }, Substitute { type Functor = F }, AsFormula { type Functor = F }}](
    judgement: Judgement[T],
    rule: InferenceRule[Judgement, F],
  ): Option[Proof[Judgement[T]]] =
    for
      unification1 <- rule.conclusion.assertion.unify(judgement.assertion)
      unification2 <- rule.conclusion.assumptions.toSeq.unify(judgement.assumptions.toSeq)
      conclusion   <- rule.conclusion.assertion.substitute(unification1)
      assumptions  <- rule.conclusion.assumptions.toSeq.substitute(unification2)
      hypotheses   <- rule.hypotheses.toSeq.traverse { hypothesis =>
        for
          assertion   <- hypothesis.assertion.substitute(unification1)
          assumptions <- hypothesis.assumptions.toSeq.substitute(unification2)
        yield Judgement(assumptions.toSet, assertion)
      }
    yield Proof(Judgement(assumptions.toSet, conclusion), hypotheses.map(Proof(_, List.empty)).toList)
