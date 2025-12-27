package proofPlayground
package core.logic.propositional

import core.logic.symbol

/** Representation of a propositional formula. */
case class Formula(formula: FormulaF[Formula])

case object Formula:
  /** Marker trait for propositional logic variables. */
  sealed trait Propositional

/** Representation of the structure of a propositional logic formula.
 *
 * @tparam F the type used for recursive positions.
 */
enum FormulaF[F]:

  /** A propositional variable. */
  case Variable(variable: symbol.Variable[Formula.Propositional])

  /** The true constant. */
  case True(tru: symbol.True)

  /** The false constant. */
  case False(fls: symbol.False)

  /** The negation of a formula. */
  case Negation(negation: symbol.Negation[F])

  /** The conjunction of two formulas. */
  case Conjunction(conjunction: symbol.Conjunction[F])

  /** The disjunction of two formulas. */
  case Disjunction(disjunction: symbol.Disjunction[F])

  /** The implication from a formula to a formula. */
  case Implication(implication: symbol.Implication[F])
