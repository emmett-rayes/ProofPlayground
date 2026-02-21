package proofPlayground
package core.proof

import zipper.TreeZipper.given
import zipper.{Tree, TreeZipper, Zipper}

/** Representation of a proof.
  *
  * A proof has a tree structure with judgements as nodes.
  * A proof is considered valid if each level in each branch of the tree
  * is justified by an inference rule.
  *
  * @tparam J the type of judgements used in the proof.
  */
opaque type Proof[J] = Tree[J]

/** A zipper over a proof.
  *
  * Allows for navigating the proof and modifying it.
  *
  * @tparam J the type of judgements used in the proof.
  */
opaque type ProofZipper[J] = TreeZipper[J]

object Proof {
  def apply[J](root: J, subproofs: List[Proof[J]]): Proof[J] = Tree(root, subproofs)

  def unapply[J](proof: Proof[J]): Option[(J, List[Proof[J]])] = Some(proof.value, proof.children)

  extension [J](proof: Proof[J]) {

    /** Returns the conclusion of the proof. */
    def conclusion: J = proof.value

    /** Returns the recursive sub-derivations leading to the conclusion. */
    def subproofs: List[Proof[J]] = proof.children

    /** Returns the proof tree representation of the proof. */
    def asTree: Tree[J] = proof

    /** Returns a zipper over the proof tree. */
    def zipper: ProofZipper[J] = ProofZipper(proof)
  }
}

object ProofZipper {
  def apply[J](proof: Proof[J]): ProofZipper[J] = TreeZipper(proof)

  given ProofZipper is Zipper[Proof] = summon[Zipper[Proof]]
}
