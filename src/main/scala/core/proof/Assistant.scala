package proofPlayground
package core.proof

import core.meta.Substitute.{substitute, substitutePartial}
import core.meta.Unification.merge
import core.meta.Unifier.given
import core.meta.{AsPattern, CaptureAvoidingSub, FreeVars, MetaVariable, Unification, Unifier}
import core.proof.natural.Judgement
import core.{Algebra, Fix, Functor, traverse}
import core.meta.Pattern.given

object Assistant {

  /** Attempts to produce a proof for the given judgement by applying the given inference rule.
    *
    * The proof is constructible if the judgement can be proved by applying the inference rule to the judgement.
    * Unification is used to match the patterns in the inference rule with the concrete formulas in the judgement.
    * Substitution is used to substitute the unification results into the patterns in the inference rule.
    * Thus producing new premises as given by the inference rule.
    *
    * @tparam F The type of the formula functor used in patterns in the inference rule.
    * @param judgement The judgement to be proved.
    * @param rule The inference rule to be applied.
    * @param auxUnification A unification for the meta-variables appearing in the premises but not in the conclusion.
    * @return Some(proof) if it is constructible, None otherwise.
    */
  def proof[F[_]: Functor](using
                           Algebra[F, Option[Fix[F]]],
                           Algebra[F, Unifier[Fix[F]]#Fn],
                           Algebra[F, Set[MetaVariable]],
                           FreeVars { type Self = Fix[F] },
                           AsPattern[F] { type Self = Fix[F] },
                           CaptureAvoidingSub { type Self = Fix[F] },
  )(
    judgement: Judgement[Fix[F]],
    rule: InferenceRule[Judgement, F],
    auxUnification: Unification[Fix[F]] = Map.empty[MetaVariable, Fix[F]]
  ): ProofResult[Judgement, F] = {
    val sideCondition = judgement.free.find { free =>
      judgement.assertion.freevariables.contains(free)
    }
    if sideCondition.isDefined then return ProofResult.SideConditionFailure(sideCondition.get)

    val unificationOpt =
      for
        assertionUnification       <- rule.conclusion.assertion.unifier(judgement.assertion)
        assumptionsUnification     <- rule.conclusion.assumptions.toSeq.unifier(judgement.assumptions.toSeq)
        freeUnification            <- rule.conclusion.free.toSeq.unifier(judgement.free.toSeq)
        totalUnification           <- merge(assertionUnification, auxUnification)
        totalAssumptionUnification <- merge(assumptionsUnification, totalUnification)
        totalFreeUnification       <- merge(freeUnification, totalUnification)
      yield (totalUnification, totalAssumptionUnification, totalFreeUnification)
    if unificationOpt.isEmpty then return ProofResult.UnificationFailure()

    val (unification, assumptionUnification, freeUnification)                                 = unificationOpt.get
    val proofOrFailure: Option[Either[InferenceRule[Judgement, F], Proof[Judgement[Fix[F]]]]] =
      for
        conclusion <- substitute[Fix[F], F](rule.conclusion, unification, assumptionUnification, freeUnification)
        premises   <- rule.premises.toSeq.traverse { premise =>
          substitute[Fix[F], F](premise, unification, assumptionUnification, freeUnification)
        }
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

    val substitutedRule = rule.map { j => substitutePartial(j, unification, assumptionUnification, freeUnification) }
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

    /** Boundary condition failure during proof construction.
      *
      * @param variable the variable that violates the boundary condition.
      */
    case SideConditionFailure(variable: Fix[F])

    /** Substitution failure during proof construction.
      *
      * @param partiallySubstitutedRule the inference rule that could not be fully substituted.
      */
    case SubstitutionFailure(partiallySubstitutedRule: InferenceRule[J, F])
  }
}
