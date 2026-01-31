package proofPlayground
package core.meta

import core.logic.propositional.FormulaF
import core.meta.Pattern
import core.{Algebra, catamorphism}

/** A typeclass for extracting meta-variables from a pattern (container). */
trait MetaVars:
  type Self

  extension (self: Self)
    /** Returns the set of meta-variables appearing in `self`. */
    def metavariables: Set[MetaVariable]

object MetaVars:
  type FormulaPattern = Pattern[FormulaF] // scalafmt does not handle type constructors in `is` clauses well

  private def algebra(formula: FormulaF[Set[MetaVariable]]): Set[MetaVariable] =
    formula match
      case FormulaF.Variable(variable)       => Set.empty
      case FormulaF.True(tru)                => Set.empty
      case FormulaF.False(fls)               => Set.empty
      case FormulaF.Negation(negation)       => negation.arg
      case FormulaF.Conjunction(conjunction) => conjunction.lhs ++ conjunction.rhs
      case FormulaF.Disjunction(disjunction) => disjunction.lhs ++ disjunction.rhs
      case FormulaF.Implication(implication) => implication.lhs ++ implication.rhs

  private def algebra[F[_]](subalgebra: Algebra[F, Set[MetaVariable]])(pattern: PatternF[F, Set[MetaVariable]])
    : Set[MetaVariable] =
    pattern match
      case pattern @ PatternF.Meta(_) => Set(pattern)
      case PatternF.Formula(formula)  => subalgebra(formula)

  given FormulaPattern is MetaVars:
    extension (pattern: Pattern[FormulaF])
      override def metavariables: Set[MetaVariable] =
        catamorphism(pattern)(algebra(algebra))
