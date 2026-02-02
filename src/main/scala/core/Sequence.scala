package proofPlayground
package core

import core.logic.propositional.FormulaF
import core.logic.symbol

/** A typeclass for sequencing two functors. */
trait Sequence[F1[_], F2[_]]:
  extension [T](self: F1[F2[T]])
    /** Returns `self` sequenced with `F2`. */
    def sequence: F2[F1[T]]

object Sequence:
  given Sequence[FormulaF, Option]:
    extension [T](formula: FormulaF[Option[T]])
      override def sequence: Option[FormulaF[T]] =
        formula match
          case FormulaF.Variable(sym)      => Some(FormulaF.Variable(sym))
          case FormulaF.True(sym)          => Some(FormulaF.True(sym))
          case FormulaF.False(sym)         => Some(FormulaF.False(sym))
          case FormulaF.Negation(negation) =>
            negation.arg.map(arg => FormulaF.Negation(symbol.Negation(arg)))
          case FormulaF.Conjunction(conjunction) =>
            for
              lhs <- conjunction.lhs
              rhs <- conjunction.rhs
            yield FormulaF.Conjunction(symbol.Conjunction(lhs, rhs))
          case FormulaF.Disjunction(disjunction) =>
            for
              lhs <- disjunction.lhs
              rhs <- disjunction.rhs
            yield FormulaF.Disjunction(symbol.Disjunction(lhs, rhs))
          case FormulaF.Implication(implication) =>
            for
              lhs <- implication.lhs
              rhs <- implication.rhs
            yield FormulaF.Implication(symbol.Implication(lhs, rhs))
