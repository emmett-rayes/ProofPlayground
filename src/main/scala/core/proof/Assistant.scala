package proofPlayground
package core.proof

import core.Traverse.traverse
import core.meta.*
import core.meta.Substitute.substitute
import core.meta.Unify.unify
import core.proof.natural.Judgement

object Assistant:
  def proof[F[_], T: {Unify { type Functor = F }, Substitute { type Functor = F }, Unpattern { type Functor = F }}](
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
