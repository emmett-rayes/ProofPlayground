package proofPlayground
package core.meta

import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern.given

import scala.language.implicitConversions

/** A typeclass for converting a concrete formula type to a [[Pattern]].
  *
  * @tparam F The type of the concrete formula.
  */
trait AsPattern[F[_]]:
  type Self

  extension (self: Self)
    /** Converts a concrete formula of type `Self` into a `PatternF.Concrete[F]`.
      *
      * This allows using concrete formulas directly in pattern matching tests.
      *
      * @return A `Pattern` wrapping the formula
      */
    def asPattern: Pattern[F]

object AsPattern:
  given Conversion[FormulaF[Pattern[FormulaF]], Pattern[FormulaF]] =
    PatternF.concrete(_)

  given Formula is AsPattern[FormulaF]:
    extension (formula: Formula)
      override def asPattern: Pattern[FormulaF] =
        formula.unfix match
          case FormulaF.Variable(symbol)         => variable(symbol)
          case FormulaF.True(_)                  => tru
          case FormulaF.False(_)                 => fls
          case FormulaF.Negation(negation)       => ~negation.arg.asPattern
          case FormulaF.Conjunction(conjunction) => conjunction.lhs.asPattern /\ conjunction.rhs.asPattern
          case FormulaF.Disjunction(disjunction) => disjunction.lhs.asPattern \/ disjunction.rhs.asPattern
          case FormulaF.Implication(implication) => implication.lhs.asPattern --> implication.rhs.asPattern
