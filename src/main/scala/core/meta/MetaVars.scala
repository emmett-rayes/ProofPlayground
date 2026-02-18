package proofPlayground
package core.meta

/** A typeclass for extracting meta-variables from a container. */
trait MetaVars {
  type Self

  extension (self: Self) {

    /** Returns the set of meta-variables appearing in `self`. */
    def metavariables: Set[MetaVariable]
  }
}
