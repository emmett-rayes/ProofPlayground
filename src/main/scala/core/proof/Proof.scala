package proofPlayground
package core.proof

/** Representation of a proof in natural deduction.
 *
 * A proof in natural deduction has a tree structure with judgements
 * as nodes. A proof is considered valid if each level in each branch
 * of the tree is justified by an inference rule.
 *
 * @tparam J the type of judgements used in the proof.
 * @param root      the conclusion of this proof step.
 * @param subproofs the recursive sub-derivations leading to the conclusion.
 */
case class Proof[J](root: J, subproofs: Set[J])
