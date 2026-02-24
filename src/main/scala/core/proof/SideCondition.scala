package proofPlayground
package core.proof

import core.meta.FreeVars

trait SideCondition[F: FreeVars] {
  type Self

  extension (self: Self) {
    /** Returns whether the proof step is open */
    def open: Boolean

    /** Returns the collection of formulas that violate the side condition, if any. */
    def violations: Seq[F]
  }
}
