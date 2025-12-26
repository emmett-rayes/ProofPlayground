package proofPlayground
package core.system.natural

/** Representation of a proof in natural deduction.
 *
 * A proof in natural deduction has a tree structure with judgements
 * as nodes. A proof is considered valid if each level in each branch
 * of the tree is justified by an inference rule.
 *
 * @tparam F the type of formulas used in the proof
 * @param root      the conclusion of this proof step
 * @param subproofs the set of sub-derivations leading to the conclusion
 */
case class Proof[F](root: Judgement[Seq, F], subproofs: Set[Proof[F]])
