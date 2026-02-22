package proofPlayground
package core.proof

import core.{Fix, Functor, traverse}
import core.meta.{AsPattern, MapUnification, MetaVariable, Substitute}
import core.proof.SideCondition

object Assistant {

  /** Attempts to produce a proof for the given judgement by applying the given inference rule.
    *
    * The proof is constructible if the judgement can be proved by applying the inference rule to the judgement.
    * Substitution is used to substitute the unification results into the patterns in the inference rule.
    * Thus producing new premises as given by the inference rule.
    *
    * @tparam F The type of the formula functor used in patterns in the inference rule.
    * @tparam J The type of the judgement functor used in the inference rule.
    * @param judgement The judgement to be proved.
    * @param rule The inference rule to be applied.
    * @param auxUnification A unification for the meta-variables appearing in the premises but not in the conclusion.
    * @return A result indicating whether the proof was constructible, or if there was a failure.
    */
  def proof[F[_]: Functor, J[_]: {Functor, Substitute[Fix[F], F]}](using
    Fix[F] is AsPattern[F],
    J[Fix[F]] is SideCondition[Fix[F]],
  )(
    judgement: J[Fix[F]],
    rule: InferenceRule[J, F],
    auxUnification: MapUnification[Fix[F]] = Map.empty[MetaVariable, Fix[F]],
  ): ProofResult[J, F] = {

    val violations = judgement.violations
    if violations.nonEmpty then return ProofResult.SideConditionFailure(violations)

    val conclusionUnificationOpt =
      for
        unification      <- rule.conclusion.unifier(judgement)
        totalUnification <- unification.merge(auxUnification)
      yield totalUnification
    if conclusionUnificationOpt.isEmpty then return ProofResult.UnificationFailure()

    val conclusionUnification = conclusionUnificationOpt.get
    val proofOrFailure        =
      for
        conclusion <- rule.conclusion.substitute(conclusionUnification)
        premises   <- rule.premises.toSeq.traverse { premise => premise.substitute(conclusionUnification) }
      yield
        if conclusion == judgement then
          Right(Proof(conclusion, premises.reverse.map(Proof(_, List.empty)).toList))
        else
          Left(Inference(rule.label, premises, conclusion).map(j => j.map(_.asPattern)))
    if proofOrFailure.isDefined then
      return proofOrFailure.get match {
        case Right(proof) => ProofResult.Success(proof)
        case Left(rule)   => ProofResult.SubstitutionFailure(rule)
      }

    val substitutedRule = rule.map(_.substitutePartial(conclusionUnification))
    ProofResult.SubstitutionFailure(substitutedRule)
  }

  /** Result of attempting to construct a proof. */
  enum ProofResult[J[_], F[_]] {

    /** Successful proof construction.
      *
      * @param proof the constructed proof.
      */
    case Success(proof: Proof[J[Fix[F]]])

    /** Unification failure during proof construction. */
    case UnificationFailure()

    /** Side-condition failure during proof construction.
      *
      * @param variable the variable that violates the side-condition.
      */
    case SideConditionFailure(variable: Seq[Fix[F]])

    /** Substitution failure during proof construction.
      *
      * @param partiallySubstitutedRule the inference rule that could not be fully substituted.
      */
    case SubstitutionFailure(partiallySubstitutedRule: InferenceRule[J, F])
  }
}
