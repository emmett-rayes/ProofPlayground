package proofPlayground
package core.proof

trait ClosedQuery {
  type Self

  extension (self: Self) {
    /** Returns whether the proof step is closed */
    def closed: Boolean
  }
}
