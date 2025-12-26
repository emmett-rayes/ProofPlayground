package proofPlayground
package core.logic.propositional

import core.logic.symbol

/** Marker trait for propositional logic variables. */
sealed trait Propositional

/** Represents propositional logic formulas. */
enum Formula:

  /** A propositional variable.
   *
   * @param variable the variable.
   */
  case Variable(variable: symbol.Variable[Propositional])

  /** The true constant.
   *
   * @param tru the true symbol.
   */
  case True(tru: symbol.True)

  /** The false constant.
   *
   * @param fls the false symbol.
   */
  case False(fls: symbol.False)

  /** The negation of a formula.
   *
   * @param negation the negation symbol.
   */
  case Negation(negation: symbol.Negation[Formula])

  /** The conjunction of two formulas.
   *
   * @param conjunction the conjunction symbol.
   */
  case Conjunction(conjunction: symbol.Conjunction[Formula])

  /** The disjunction of two formulas.
   *
   * @param disjunction the disjunction symbol.
   */
  case Disjunction(disjunction: symbol.Disjunction[Formula])

  /** The implication from a formula to a formula.
   *
   * @param implication the implication symbol.
   */
  case Implication(implication: symbol.Implication[Formula])
