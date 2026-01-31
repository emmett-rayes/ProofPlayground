package proofPlayground
package core.proof

import core.Traverse.traverse
import core.meta.*
import core.meta.Substitute.substitute
import core.meta.Unify.{merge, unify}
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
    * @param unification A unification for the meta-variables appearing in the hypotheses but not in the conclusion.
    * @return Some(proof) if it is constructible, None otherwise.
    */
  def proof[F[_], T: {Unify { type Functor = F }, Substitute { type Functor = F }, AsFormula { type Functor = F }}](
    judgement: Judgement[T],
    rule: InferenceRule[Judgement, F],
    unification: Unification[T] = Map.empty
  ): Option[Proof[Judgement[T]]] =
    for
      assertionUnification   <- rule.conclusion.assertion.unify(judgement.assertion): Option[Unification[T]]
      assumptionsUnification <- rule.conclusion.assumptions.toSeq.unify(judgement.assumptions.toSeq)
      totalUnification       <- merge(assertionUnification, unification)
      totalSeqUnification    <- merge(assumptionsUnification, totalUnification)
      conclusion             <- rule.conclusion.assertion.substitute(totalUnification)
      assumptions            <- rule.conclusion.assumptions.toSeq.substitute(totalSeqUnification)
      hypotheses             <- rule.hypotheses.toSeq.traverse { hypothesis =>
        for
          assertion   <- hypothesis.assertion.substitute(totalUnification)
          assumptions <- hypothesis.assumptions.toSeq.substitute(totalSeqUnification)
        yield Judgement(assumptions.toSet, assertion)
      }
    yield Proof(Judgement(assumptions.toSet, conclusion), hypotheses.map(Proof(_, List.empty)).toList)
