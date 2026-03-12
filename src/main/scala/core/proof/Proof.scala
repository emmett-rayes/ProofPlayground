package proofPlayground
package core.proof

import core.Fix
import zipper.TreeZipper.given
import zipper.{Tree, TreeZipper, Zipper}

/** A node in a proof tree, containing a judgement and its subderivation. */
case class ProofNode[F[_], J[_]](judgement: J[Fix[F]], rule: Option[InferenceRule[J, F]])

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
    Tree(ProofNode(judgement, None), List.empty)

  def apply[F[_], J[_]](judgement: J[Fix[F]], rule: InferenceRule[J, F], subproofs: List[Proof[F, J]]): Proof[F, J] =
    Tree(ProofNode(judgement, Some(rule)), subproofs)

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
