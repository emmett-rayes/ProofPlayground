package proofPlayground
package core.proof.natural

/** Representation of a judgement in natural deduction.
  *
  * A judgement consists of a sequence of assumptions and an assertion.
  * A judgement in natural deduction has a single formula in the conclusion.
  *
  * @tparam F the type of formulas.
  * @param assertion   the formula that is asserted.
  * @param assumptions the collection of formulas assumed to be true.
  * @param free the collection of variables that are not allowed to appear in the conclusion.
  *             this is used for the side conditions of existential and universal quantifiers.
  */
case class Judgement[F](assertion: F, assumptions: Seq[F], free: Seq[F])

case object Judgement:

  opaque type Context[F] = (Seq[F], Seq[F])

  /** Extension methods for judgements.
    *
    * Provides DSL for constructing judgements.
    */
  extension [F](assumptions: Seq[F])
    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, assumptions, Seq.empty)

  extension [F](free: Seq[F])
    /** Infix operator for combining assumptions and free sequences */
    def %(assumptions: Seq[F]): Context[F] = (free, assumptions)

  extension [F](context: Context[F])
    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, context._2, context._1)
