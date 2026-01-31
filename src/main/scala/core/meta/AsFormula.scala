package proofPlayground
package core.meta

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.{Algebra, catamorphism}

/** A typeclass for converting formula patterns to concrete formulas. */
trait AsFormula:
  /** The type of the concrete formula produced by the conversion. */
  type Self

  /** The type of the formula functor used in the pattern. */
  type Functor[_]

  extension (pattern: Pattern[Functor])
    /** Converts a pattern to a concrete formula.
      *
      * @note can be used only if the pattern is a [[PatternF.Formula]]
      * @param pattern the pattern to convert to a concrete formula
      * @return Some(concrete) if the conversion is successful; None otherwise
      */
    // noinspection ScalaDocUnknownParameter
    def asFormula: Option[Self]

object AsFormula:
  /** [[AsFormula]] instance for [[Formula]]. */
  given Formula is AsFormula:
    override type Functor = FormulaF
    extension (pattern: Pattern[FormulaF])
      override def asFormula: Option[Formula] =
        catamorphism(pattern)(algebra(algebra))

    /** Conversion algebra for the [[FormulaF]] functor with carrier `Option[Formula]`. */
    private def algebra(formula: FormulaF[Option[Formula]]): Option[Formula] =
      formula match
        case FormulaF.Variable(symbol)         => Some(variable(symbol))
        case FormulaF.True(_)                  => Some(tru)
        case FormulaF.False(_)                 => Some(fls)
        case FormulaF.Negation(negation)       => negation.arg.map(arg => ~arg)
        case FormulaF.Conjunction(conjunction) =>
          for
            lhs <- conjunction.lhs
            rhs <- conjunction.rhs
          yield lhs /\ rhs
        case FormulaF.Disjunction(disjunction) =>
          for
            lhs <- disjunction.lhs
            rhs <- disjunction.rhs
          yield lhs /\ rhs
        case FormulaF.Implication(implication) =>
          for
            lhs <- implication.lhs
            rhs <- implication.rhs
          yield lhs /\ rhs

    /** Conversion algebra for the [[PatternF]] functor with carrier `Option[T]`. */
    private def algebra[F[_], T](subalgebra: Algebra[F, Option[T]])(pattern: PatternF[F, Option[T]]): Option[T] =
      pattern match
        case PatternF.Meta(name)       => None
        case PatternF.Formula(formula) => subalgebra(formula)
