package proofPlayground
package core.proof.natural

import core.Functor
import core.meta.{MetaVariable, MetaVars}

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

case object Judgement {

  opaque type Context[F] = (Seq[F], Seq[F])

  /** [[Functor]] instance for [[Judgement]]. */
  given Functor[Judgement] {
    extension [A](judgement: Judgement[A]) {
      override def map[B](f: A => B): Judgement[B] =
        Judgement(f(judgement.assertion), judgement.assumptions.map(f), judgement.free.map(f))
    }
  }

  /** [[MetaVars]] instance for [[Judgement]]. */
  given [F: MetaVars] => Judgement[F] is MetaVars {
    extension (judgement: Judgement[F]) {
      override def metavariables: Set[MetaVariable] =
        judgement.assertion.metavariables ++ judgement.assumptions.flatMap(_.metavariables)
    }
  }

  extension [F](assumptions: Seq[F]) {

    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, assumptions, Seq.empty)
  }

  extension [F](free: Seq[F]) {

    /** Infix operator for combining assumptions and free sequences */
    def %(assumptions: Seq[F]): Context[F] = (free, assumptions)
  }

  extension [F](context: Context[F]) {

    /** Judgement infix constructor. */
    def |-(assertion: F): Judgement[F] = Judgement(assertion, context._2, context._1)
  }
}
