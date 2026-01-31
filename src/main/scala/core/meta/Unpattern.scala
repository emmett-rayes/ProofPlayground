package proofPlayground
package core.meta

import core.{Algebra, Fix, catamorphism}
import core.logic.propositional.{Formula, FormulaF}
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.FormulaF.{fls, tru, variable}

trait Unpattern:
  type Self
  type Functor[_]

  extension (pattern: Pattern[Functor])
    def unpattern: Option[Self]

object Unpattern:
  given Formula is Unpattern:
    override type Functor = FormulaF

    extension (pattern: Pattern[FormulaF])
      override def unpattern: Option[Formula] =
        catamorphism(pattern)(algebra(algebra))

    private def algebra(formula: FormulaF[Option[Fix[FormulaF]]]): Option[Fix[FormulaF]] =
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

    private def algebra[F[_]](subalgebra: Algebra[F, Option[Fix[F]]])(pattern: PatternF[F, Option[Fix[F]]])
      : Option[Fix[F]] =
      pattern match
        case PatternF.Meta(name)       => None
        case PatternF.Formula(formula) => subalgebra(formula)
