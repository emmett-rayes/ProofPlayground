package proofPlayground
package frontend.tui

import core.logic.propositional.{Formula, FormulaF}
import core.catamorphism

import core.logic.propositional

object Show:
  def show(formula: Formula): String =
    catamorphism(formula)(algebra)

  private def algebra(formula: FormulaF[String]): String =
    formula match
      case propositional.FormulaF.Variable(variable)       => variable.id
      case propositional.FormulaF.True(tru)                => "⊤"
      case propositional.FormulaF.False(fls)               => "⊥"
      case propositional.FormulaF.Negation(negation)       => s"¬${negation.arg}"
      case propositional.FormulaF.Conjunction(conjunction) => s"(${conjunction.lhs} ∧ ${conjunction.rhs})"
      case propositional.FormulaF.Disjunction(disjunction) => s"(${disjunction.lhs} ∨ ${disjunction.rhs})"
      case propositional.FormulaF.Implication(implication) => s"(${implication.lhs} → ${implication.rhs})"
