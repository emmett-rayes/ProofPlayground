package proofPlayground
package core.meta

trait CaptureAvoidingSub {
  type Self

  extension (self: Self) {

    /** Performs capture-avoiding substitution of `term` for `variable` in `self`.
      *
      * @param variable the variable to be substituted in `self`.
      * @param replacement the term to substitute for `variable` in `self`.
      * @return a new value with `term` substituted for `variable`, avoiding variable capture.
      */
    def substituteWithoutCapturing(variable: Self, replacement: Self): Self
  }
}
