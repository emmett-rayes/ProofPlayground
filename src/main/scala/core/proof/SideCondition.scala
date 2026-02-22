package proofPlayground
package core.proof

import core.meta.FreeVars

trait SideCondition[F: FreeVars] {
  type Self

  /** Returns the collection of formulas that violate the side condition, if any. */
  extension (self: Self) {
    def violations: Seq[F]
  }
}
