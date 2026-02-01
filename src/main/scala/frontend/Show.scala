package proofPlayground
package frontend

import core.catamorphism
import core.logic.propositional
import core.logic.propositional.{Formula, FormulaF}
import core.proof.natural.Judgement

/** A typeclass for showing a value of type `Self` as a string. */
trait Show:
  type Self

  extension (self: Self)
    /** Returns a string representation of `self`. */
    def show: String

object Show:
  private def algebra(formula: FormulaF[String]): String =
    formula match
      case propositional.FormulaF.Variable(variable)       => variable.id
      case propositional.FormulaF.True(tru)                => "⊤"
      case propositional.FormulaF.False(fls)               => "⊥"
      case propositional.FormulaF.Negation(negation)       => s"¬${negation.arg}"
      case propositional.FormulaF.Conjunction(conjunction) => s"(${conjunction.lhs} ∧ ${conjunction.rhs})"
      case propositional.FormulaF.Disjunction(disjunction) => s"(${disjunction.lhs} ∨ ${disjunction.rhs})"
      case propositional.FormulaF.Implication(implication) => s"(${implication.lhs} → ${implication.rhs})"

  /** [[Show]] instance for [[Formula]]. */
  given Formula is Show:
    extension (formula: Self)
      override def show: String =
        catamorphism(formula)(algebra)

  /** [[Show]] instance for [[Judgement]]. */
  given [F: Show] => Judgement[F] is Show:
    extension (judgement: Judgement[F])
      override def show: String =
        s"${judgement.assumptions.map(_.show).mkString(",")} ⊢ ${judgement.assertion.show}"
