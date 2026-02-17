package proofPlayground
package frontend

import core.logic.propositional.{Formula, FormulaF}
import core.meta.{Pattern, PatternF}
import core.proof.natural.Judgement
import core.{Algebra, Functor, catamorphism}

/** A typeclass for showing a value of type `Self` as a string. */
trait Show:
  type Self

  extension (self: Self)
    /** Returns a string representation of `self`. */
    def show: String

object Show:
  given Algebra[FormulaF, String] = {
    case FormulaF.Variable(variable)       => variable.id
    case FormulaF.True(tru)                => "⊤"
    case FormulaF.False(fls)               => "⊥"
    case FormulaF.Negation(negation)       => s"¬${negation.arg}"
    case FormulaF.Conjunction(conjunction) => s"(${conjunction.lhs} ∧ ${conjunction.rhs})"
    case FormulaF.Disjunction(disjunction) => s"(${disjunction.lhs} ∨ ${disjunction.rhs})"
    case FormulaF.Implication(implication) => s"(${implication.lhs} → ${implication.rhs})"
    case FormulaF.Universal(universal)     => s"(∀${universal.variable}.${universal.body})"
    case FormulaF.Existential(existential) => s"(∃${existential.variable}.${existential.body})"
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
        val algebra = Pattern.algebra(summon) {
          case PatternF.Meta(name)                                   => name
          case PatternF.Substitution(variable, replacement, formula) => s"$formula[$replacement/$variable]"
        }
        val result = catamorphism(pattern)(algebra).stripPrefix("(").stripSuffix(")")
        if result.startsWith("(") && result.endsWith(")")
        then result.stripPrefix("(").stripSuffix(")")
        else result

  /** [[Show]] instance for [[Judgement]]. */
  given [F: Show] => Judgement[F] is Show:
    extension (judgement: Judgement[F])
      override def show: String =
        val assertion   = judgement.assertion.show
        val assumptions = judgement.assumptions.map(_.show).mkString(", ")
        val free        = judgement.free.map(_.show).mkString(", ")
        val lhs         = if assumptions.nonEmpty && free.nonEmpty then s"$free ; $assumptions" else free + assumptions
        s"$lhs ⊢ $assertion"
