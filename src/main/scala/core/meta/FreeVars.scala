package proofPlayground
package core.meta

import core.logic.propositional.Formula.given
import core.logic.propositional.{Formula, FormulaF}
import core.{Algebra, catamorphism}

import scala.language.implicitConversions

/** A typeclass for extracting free-variables from a formula. */
trait FreeVars {
  type Self

  extension (self: Self) {

    /** Returns the set of meta-variables appearing in `self`. */
    def freevariables: Set[Self]
  }
}
