package proofPlayground
package core.proof

import core.meta.Substitute.{substitute, substitutePartial}
import core.meta.Unification.merge
import core.meta.Unify.{Unifier, unify}
import core.meta.{AsPattern, MetaVariable, Unification}
import core.proof.natural.Judgement
import core.{Algebra, Fix, Functor, traverse}

object Assistant:
  /** Attempts to produce a proof for the given judgement by applying the given inference rule.
    *
    * The proof is constructible if the judgement can be proved by applying the inference rule to the judgement.
    * Unification is used to match the patterns in the inference rule with the concrete formulas in the judgement.
    * Substitution is used to substitute the unification results into the patterns in the inference rule.
    * Thus producing new hypotheses as given by the inference rule.
    *
    * @tparam F The type of the formula functor used in patterns in the inference rule.
    * @param judgement The judgement to be proved.
    * @param rule The inference rule to be applied.
    * @param auxUnification A unification for the meta-variables appearing in the hypotheses but not in the conclusion.
    * @return Some(proof) if it is constructible, None otherwise.
    */
  def proof[F[_]: Functor](using
    Algebra[F, Option[Fix[F]]],
    Algebra[F, Unifier[Fix[F]]],
    Algebra[F, Set[MetaVariable]],
    AsPattern[F] { type Self = Fix[F] },
  )(
    judgement: Judgement[Fix[F]],
    rule: InferenceRule[Judgement, F],
    auxUnification: Unification[Fix[F]] = Map.empty[MetaVariable, Fix[F]]
  ): ProofResult[Judgement, F] =
    val unificationOpt =
      for
        assertionUnification   <- unify(rule.conclusion.assertion, judgement.assertion): Option[Unification[Fix[F]]]
        assumptionsUnification <- unify[Fix[F], F](rule.conclusion.assumptions.toSeq, judgement.assumptions.toSeq)
        totalUnification       <- merge(assertionUnification, auxUnification)
        totalSeqUnification    <- merge(assumptionsUnification, totalUnification)
      yield (totalUnification, totalSeqUnification)

    if unificationOpt.isEmpty then ProofResult.UnificationFailure()
    else
      val (unification, seqUnification) = unificationOpt.get

      val proof =
        for
          assertion   <- substitute[Fix[F], F](rule.conclusion.assertion, unification)
          assumptions <- substitute[Fix[F], F](rule.conclusion.assumptions.toSeq, seqUnification)
          hypotheses  <- rule.hypotheses.toSeq.traverse { hypothesis =>
            for
              assertion   <- substitute[Fix[F], F](hypothesis.assertion, unification)
              assumptions <- substitute[Fix[F], F](hypothesis.assumptions.toSeq, seqUnification)
            yield Judgement(assumptions, assertion)
          }
        yield Proof(Judgement(assumptions, assertion), hypotheses.reverse.map(Proof(_, List.empty)).toList)

      if proof.isDefined then ProofResult.Success(proof.get)
      else
        val assertion   = substitutePartial(rule.conclusion.assertion, unification)
        val assumptions = substitutePartial(rule.conclusion.assumptions.toSeq, seqUnification)
        val hypotheses  = rule.hypotheses.map { hypothesis =>
          val assertion   = substitutePartial(hypothesis.assertion, unification)
          val assumptions = substitutePartial(hypothesis.assumptions.toSeq, seqUnification)
          Judgement(assumptions, assertion)
        }
        val conclusion  = Judgement(assumptions, assertion)
        ProofResult.SubstitutionFailure(Inference(rule.label, hypotheses, conclusion))

  /** Result of attempting to construct a proof. */
  enum ProofResult[J[_], F[_]]:
    /** Successful proof construction.
      *
      * @param proof the constructed proof.
      */
    case Success(proof: Proof[J[Fix[F]]])

    /** Unification failure during proof construction. */
    case UnificationFailure()

    /** Substitution failure during proof construction.
      *
      * @param partiallySubstitutedRule the inference rule that could not be fully substituted.
      */
    case SubstitutionFailure(partiallySubstitutedRule: InferenceRule[J, F])
