package proofPlayground
package core.meta

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.meta.{Pattern, PatternF}
import core.{Algebra, catamorphism}

object Substitution:
  def substitute(pattern: Pattern[FormulaF], unification: Unification[Formula]): Option[Formula] =
    val subalgebra = algebra(unification)
    catamorphism(pattern)(algebra(subalgebra)(unification))

  def algebra[T](unification: Unification[Formula])(formula: FormulaF[Option[Formula]]): Option[Formula] =
    formula match
      case FormulaF.Variable(symbol)         => Some(variable(symbol))
      case FormulaF.True(_)                  => Some(tru)
      case FormulaF.False(_)                 => Some(fls)
      case FormulaF.Negation(negation)       => negation.arg.map(arg => ~arg)
      case FormulaF.Conjunction(conjunction) => for lhs <- conjunction.lhs; rhs <- conjunction.rhs yield lhs /\ rhs
      case FormulaF.Disjunction(disjunction) => for lhs <- disjunction.lhs; rhs <- disjunction.rhs yield lhs \/ rhs
      case FormulaF.Implication(implication) => for lhs <- implication.lhs; rhs <- implication.rhs yield lhs --> rhs

  def algebra[F[_], T](subalgebra: Algebra[F, Option[T]])(unification: Unification[T])(pattern: PatternF[F, Option[T]])
    : Option[T] =
    pattern match
      case pattern @ PatternF.Meta(name) => unification.get(pattern)
      case PatternF.Formula(formula)     => subalgebra(formula)
