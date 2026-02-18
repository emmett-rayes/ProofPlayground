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

object FreeVars {
  /** Algebra for extracting free-variables from a [[FormulaF]].
    *
    * @tparam T The type of values produced by the algebra.
    */
  given [T] => (Conversion[FormulaF.Variable[?], T]) => Algebra[FormulaF, Set[T]] = {
    case variable @ FormulaF.Variable(_)   => Set(variable)
    case FormulaF.True(tru)                => Set.empty
    case FormulaF.False(fls)               => Set.empty
    case FormulaF.Negation(negation)       => negation.arg
    case FormulaF.Conjunction(conjunction) => conjunction.lhs ++ conjunction.rhs
    case FormulaF.Disjunction(disjunction) => disjunction.lhs ++ disjunction.rhs
    case FormulaF.Implication(implication) => implication.lhs ++ implication.rhs
    case FormulaF.Universal(universal)     => universal.body -- universal.variable
    case FormulaF.Existential(existential) => existential.body -- existential.variable
  }

  given Formula is FreeVars {
    extension (formula: Formula) {
      override def freevariables: Set[Formula] = {
        given Conversion[FormulaF.Variable[?], Formula] = variable =>
          FormulaF.Variable[Formula](variable.variable)
        catamorphism(formula)(summon)
      }
    }
  }
}
