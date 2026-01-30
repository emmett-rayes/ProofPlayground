package proofPlayground
package core.proof.natural

/** Representation of a judgement in natural deduction.
  *
  * A judgement consists of a set of assumptions and an assertion.
  * A judgement in natural deduction has a single formula in the conclusion.
  *
  * @tparam F the type of formulas.
  * @param assumptions the collection of formulas assumed to be true.
  * @param assertion   the formula that is asserted.
  */
case class Judgement[F](assumptions: Set[F], assertion: F)

case object Judgement:

  /** Extension methods for judgements.
    *
    * Provides DSL for constructing judgements.
    */
  extension [F](assumptions: Set[F])
    /** Judgement infix constructor. */
    def |-(assertion: F) = Judgement(assumptions, assertion)
