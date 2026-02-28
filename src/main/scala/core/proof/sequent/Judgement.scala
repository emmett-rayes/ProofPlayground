package proofPlayground
package core.proof.sequent

import core.Functor

/** Representation of a judgement in sequent calculus.
  *
  * A judgement consists of a sequence of antecedents and a sequence of succedents.
  *
  * @tparam F the type of formulas.
  * @param antecedents the collection of formulas that are assumed to be true.
  * @param succedents the collection of formulas that of which one is asserted to be true.
  */
case class Judgement[F](antecedents: Seq[F], succedents: Seq[F], nonfree: Option[F] = None)

object Judgement {

  /** [[Functor]] instance for [[Judgement]]. */
  given Functor[Judgement] {
    extension [A](judgement: Judgement[A]) {
      override def map[B](f: A => B): Judgement[B] =
        Judgement(
          judgement.antecedents.map(f),
          judgement.succedents.map(f),
        )
    }
  }

  extension [F](judgement: Judgement[F]) {

    /** Add a side-condition to the judgement. */
    def -(sidecondition: F): Judgement[F] =
      Judgement(judgement.antecedents, judgement.succedents, Some(sidecondition))
  }

  extension [F](antecedents: Seq[F]) {

    /** Judgement infix constructor. */
    def |-(succedents: Seq[F]): Judgement[F] = Judgement(antecedents, succedents)
  }
}
