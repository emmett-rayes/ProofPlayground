package proofPlayground
package core.proof

import core.Fix
import core.meta.{FreeVars, MapUnification, Unify}
import core.proof.{Assistant, ProofRequirements}
import core.proof.Assistant.ProofResult
import zipper.TreeZipper.given
import zipper.{Tree, TreeZipper, Zipper}

/** A node in a proof tree, containing a judgement and its subderivation. */
case class ProofNode[F[_], J[_]](rule: Option[InferenceRule[J, F]], judgement: J[Fix[F]], subproofs: Seq[Proof[F, J]])(
  scj: Option[J[Fix[F]]]
) {
  def asProof: Proof[F, J] = Tree(this, subproofs.toList)

  def sidecondition(using Fix[F] is FreeVars): Boolean =
    rule match {
      case None       => true
      case Some(rule) =>
        scj match {
          case None    => true
          case Some(j) => rule.sidecondition.condition(j, this.asProof)
        }
    }
}

/** Representation of a proof.
  *
  * A proof has a tree structure with judgements and rules as nodes.
  * A proof is considered valid if each level in each branch of the tree
  * is justified by the corresponding inference rule and subproofs.
  * If no rule is present, the proof is incomplete and the judgement is not justified.
  *
  * @note If no rule is present, there must be no subproofs. This invariant is not enforced by the type system.
  *
  * @tparam F the functor for the formulas used in the proof.
  * @tparam J the functor for the judgements used in the proof.
  */
type Proof[F[_], J[_]] = Tree[ProofNode[F, J]]

object Proof {
  def apply[F[_], J[_]](judgement: J[Fix[F]]): Proof[F, J] =
    ProofNode(None, judgement, List.empty)(None).asProof

  def apply[F[_], J[_]](using
    req: ProofRequirements[F, J]
  )(
    rule: InferenceRule[J, F],
    unification: req.Uni[Fix[F]],
    judgement: J[Fix[F]],
    subproofs: Seq[Proof[F, J]],
  ): Proof[F, J] = {
    import req.given
    val substituted = rule.sidecondition.metajudgement.map(_.substitute(unification)).flatten
    ProofNode(Some(rule), judgement, subproofs)(substituted).asProof
  }

  extension [F[_], J[_]](proof: Proof[F, J]) {

    /** Returns the conclusion of the proof. */
    def conclusion: J[Fix[F]] = proof.value.judgement

    /** Returns the inference rule used to derive the conclusion from the sub-derivations. */
    def rule: Option[InferenceRule[J, F]] = proof.value.rule

    /** Returns the recursive sub-derivations leading to the conclusion. */
    def subproofs: List[Proof[F, J]] = proof.children

    /** Returns the proof tree representation of the proof. */
    def asTree: Tree[ProofNode[F, J]] = proof

    /** Returns a zipper over the proof tree. */
    def zipper: ProofZipper[F, J] = ProofZipper(proof)

    /** Attempts to apply an inference rule to the proof */
    def apply(using
      req: ProofRequirements[F, J]
    )(rule: InferenceRule[J, F], unification: req.Uni[Fix[F]] = req.Uni.empty[Fix[F]]): ProofResult[F, J] =
      Assistant.proof(proof.conclusion, rule, unification)
  }
}

/** A zipper over a proof.
  *
  * Allows for navigating the proof and modifying it.
  *
  * @tparam F the functor for the formulas used in the proof.
  * @tparam J the functor for the judgements used in the proof.
  */
type ProofZipper[F[_], J[_]] = TreeZipper[ProofNode[F, J]]

object ProofZipper {
  def apply[F[_], J[_]](proof: Proof[F, J]): ProofZipper[F, J] = TreeZipper(proof)
}
