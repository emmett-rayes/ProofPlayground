package proofPlayground
package core.proof

import core.{Fix, Functor, traverse}
import core.meta.{AsPattern, FreeVars, MapUnification, MetaVariable, MetaVars, Pattern, Substitute}
import core.proof.Inference.given

/** Required capabilities for a legal formula-judgement combination. */
trait ProofRequirements[F[_], J[_]](using
  val functorF: Functor[F],
  val functorJ: Functor[J],
  val asPatternF: Fix[F] is AsPattern[F],
  val metavarsJ: J[Pattern[F]] is MetaVars,
  val substituteJ: J is Substitute[Fix[F], F],
) {
  type Uni = substituteJ.Uni
  object Uni {
    def empty[T] = substituteJ.Uni.empty[T]
  }
}

object ProofRequirements {
  given [F[_], J[_]]
    => (Functor[F])
    => (Functor[J])
    => (Fix[F] is AsPattern[F])
    => (J[Pattern[F]] is MetaVars)
    => (J is Substitute[Fix[F], F])
      => ProofRequirements[F, J] {}
}

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
  def proof[F[_], J[_]](using
    req: ProofRequirements[F, J]
  )(
    judgement: J[Fix[F]],
    rule: InferenceRule[J, F],
    auxUnification: req.Uni[Fix[F]] = req.Uni.empty[Fix[F]],
  ): ProofResult[F, J] = {
    import req.given

    val unificationResult =
      for
        conclusionUnification <- rule.conclusion.unifier(judgement)
        totalUnification      <- conclusionUnification.merge(auxUnification)
      yield totalUnification

    val unification = unificationResult.get

    val result =
      for
        conclusion <- rule.conclusion.substitute(unification)
        premises   <- rule.premises.traverse { premise => premise.substitute(unification) }
      yield {
        if conclusion == judgement then
          // it is important to return the judgement here, not the substituted conclusion, because the judgement may
          // contain further judgement specific information internally.
          val proof =
            Proof(using req)(rule, unification, judgement, premises.map(Proof.apply))
          ProofResult.Success(proof)
        else
          val substitutedRule: Inference[Fix[F], J] = Inference(rule.label, premises, conclusion)
          ProofResult.SubstitutionFailure(substitutedRule.map(_.asPattern))
      }

    def ProofError(rule: InferenceRule[J, F]): ProofResult[F, J] = {
      val conclusion      = rule.conclusion.substitutePartial(unification)
      val premises        = rule.premises.map(_.substitutePartial(unification))
      val substitutedRule = Inference(rule.label, premises, conclusion)
      // ugly hack because we do not differentiate between sequence meta-variables and formula meta-variables.
      val probablyMultiplicative =
        def condition = { (j: J[Pattern[F]]) =>
          val metavariables = j.metavariables
          metavariables.nonEmpty && metavariables.forall(v => v.name.nonEmpty && v.name.head.isUpper)
        }
        (substitutedRule.conclusion +: substitutedRule.premises).exists(condition)

      if unificationResult.isFailure && !probablyMultiplicative
      then
        ProofResult.UnificationFailure(substitutedRule)
      else
        ProofResult.SubstitutionFailure(substitutedRule)
    }

    result.getOrElse(ProofError(rule))
  }

  /** Result of attempting to construct a proof. */
  enum ProofResult[F[_], J[_]] {

    /** Successful proof construction.
      *
      * @param proof the constructed proof.
      */
    case Success(proof: Proof[F, J])

    /** Unification failure during proof construction.
      *
      * @param partiallySubstitutedRule the inference rule that could not be fully substituted due to unification failure.
      */
    case UnificationFailure(partiallySubstitutedRule: InferenceRule[J, F])

    /** Substitution failure during proof construction.
      *
      * @param partiallySubstitutedRule the inference rule that could not be fully substituted due to substitution failure.
      */
    case SubstitutionFailure(partiallySubstitutedRule: InferenceRule[J, F])
  }
}
