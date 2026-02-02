package proofPlayground
package core.proof

import core.{Algebra, Fix, Functor, Sequence, traverse}
import core.meta.*
import core.meta.MetaVars.given
import core.meta.Substitute.{substitute, substituteSeq, given}
import core.meta.Unify.{merge, unify}
import core.proof.natural.Judgement

object Assistant:
  type SequenceOption = [X[_]] =>> Sequence[X, Option]

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
  def proof[F[_]: {Functor, SequenceOption}](using
    Algebra[F, Fix[F]],
    Algebra[F, Set[MetaVariable]],
  )(using
    Unify { type Self = Fix[F]; type Functor = F },
  )(
    judgement: Judgement[Fix[F]],
    rule: InferenceRule[Judgement, F],
    auxUnification: Unification[Fix[F]] = Map.empty[MetaVariable, Fix[F]]
  ): ProofResult[Judgement[Fix[F]]] =
    val unificationOpt =
      for
        assertionUnification   <- rule.conclusion.assertion.unify(judgement.assertion): Option[Unification[Fix[F]]]
        assumptionsUnification <- rule.conclusion.assumptions.toSeq.unify(judgement.assumptions.toSeq)
        totalUnification       <- merge(assertionUnification, auxUnification)
        totalSeqUnification    <- merge(assumptionsUnification, totalUnification)
      yield (totalUnification, totalSeqUnification)

    if unificationOpt.isEmpty then ProofResult.UnificationFailure()
    else
      val (unification, seqUnification) = unificationOpt.get

      val proof =
        for
          conclusion  <- substitute[Fix[F], F](rule.conclusion.assertion, unification)
          assumptions <- substituteSeq[Fix[F], F](rule.conclusion.assumptions.toSeq, seqUnification)
          hypotheses  <- rule.hypotheses.toSeq.traverse { hypothesis =>
            for
              assertion   <- substitute[Fix[F], F](hypothesis.assertion, unification)
              assumptions <- substituteSeq[Fix[F], F](hypothesis.assumptions.toSeq, seqUnification)
            yield Judgement(assumptions.toSet, assertion)
          }
        yield Proof(Judgement(assumptions.toSet, conclusion), hypotheses.map(Proof(_, List.empty)).toList)

      if proof.isDefined then ProofResult.Success(proof.get)
      else
        val conclusionMetaVars = rule.conclusion.metavariables: Set[MetaVariable]
        val criticalMetaVars   = rule.metavariables.diff(conclusionMetaVars)
        ProofResult.SubstitutionFailure(criticalMetaVars.toSeq)

  /** Result of attempting to construct a proof. */
  enum ProofResult[J]:
    /** Successful proof construction.
      *
      * @param proof the constructed proof.
      */
    case Success(proof: Proof[J])

    /** Unification failure during proof construction. */
    case UnificationFailure()

    /** Substitution failure during proof construction.
      *
      * @param metavariables The meta-variables that could not be substituted.
      *
      * @note currently, `metavariables` contains *all* meta-variables that appear in
      *       the hypotheses of the rule but not in the conclusion, i.e. a superset of all the meta-variables.
      */
    case SubstitutionFailure(metavariables: Seq[MetaVariable])
