package proofPlayground
package core.meta

/** A typeclass for converting a concrete formula type to a [[Pattern]].
  *
  * @tparam F The type of the concrete formula.
  */
trait AsPattern[F[_]] {
  type Self

  extension (self: Self) {

    /** Converts a concrete formula of type `Self` into a `PatternF.Concrete[F]`.
      *
      * This allows using concrete formulas directly in pattern matching tests.
      *
      * @return A `Pattern` wrapping the formula
      */
    def asPattern: Pattern[F]
  }
}
