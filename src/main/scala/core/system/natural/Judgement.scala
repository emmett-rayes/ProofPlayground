package proofPlayground
package core.system.natural

/** Representation of a judgement in natural deduction.
 *
 * A judgement consists of a set of assumptions and an assertion.
 * A judgement in natural deduction has a single formula in the conclusion.
 *
 * @tparam S the type constructor for the collection of assumptions.
 * @tparam F the type of formulas.
 * @param assumptions the collection of formulas assumed to be true.
 * @param assertion   the formula that is asserted.
 */
case class Judgement[S[_], F](assumptions: S[F], assertion: F)
