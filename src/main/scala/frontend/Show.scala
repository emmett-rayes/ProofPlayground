package proofPlayground
package frontend

import core.{Algebra, Functor, catamorphism}
import core.logic.propositional
import core.logic.propositional.{Formula, FormulaF}
import core.meta.Pattern
import core.proof.natural.Judgement

/** A typeclass for showing a value of type `Self` as a string. */
trait Show:
  type Self

  extension (self: Self)
    /** Returns a string representation of `self`. */
    def show: String

object Show:
  given Algebra[FormulaF, String] = {
    case propositional.FormulaF.Variable(variable)       => variable.id
    case propositional.FormulaF.True(tru)                => "⊤"
    case propositional.FormulaF.False(fls)               => "⊥"
    case propositional.FormulaF.Negation(negation)       => s"¬${negation.arg}"
    case propositional.FormulaF.Conjunction(conjunction) => s"(${conjunction.lhs} ∧ ${conjunction.rhs})"
    case propositional.FormulaF.Disjunction(disjunction) => s"(${disjunction.lhs} ∨ ${disjunction.rhs})"
    case propositional.FormulaF.Implication(implication) => s"(${implication.lhs} → ${implication.rhs})"
  }

  /** [[Show]] instance for [[Formula]]. */
  given Formula is Show:
    extension (formula: Self)
      override def show: String =
        val result = catamorphism(formula)(summon)
        if result.startsWith("(") && result.endsWith(")")
        then result.stripPrefix("(").stripSuffix(")")
        else result

  given [F[_]: Functor] => (Algebra[F, String]) => Pattern[F] is Show:
    extension (pattern: Pattern[F])
      override def show: String =
        val algebra = Pattern.algebra(summon)(_.name)
        val result  = catamorphism(pattern)(algebra).stripPrefix("(").stripSuffix(")")
        if result.startsWith("(") && result.endsWith(")")
        then result.stripPrefix("(").stripSuffix(")")
        else result

  /** [[Show]] instance for [[Judgement]]. */
  given [F: Show] => Judgement[F] is Show:
    extension (judgement: Judgement[F])
      override def show: String =
        s"${judgement.assumptions.map(_.show).mkString(", ")} ⊢ ${judgement.assertion.show}"
