package proofPlayground
package core.logic.propositional

import core.logic.symbol
/** Marker trait for propositional logic variables */
sealed trait Propositional

/** Represents propositional logic formulas */
enum Formula:
  /** A propositional variable */
  case Variable(variable: symbol.Variable[Propositional])
  /** The true constant */
  case True(tru: symbol.True)
  /** The false constant */
  case False(fls: symbol.False)
  /** The negation of a formula */
  case Negation(negation: symbol.Negation[Formula])
  /** The conjunction of two formulas */
  case Conjunction(conjunction: symbol.Conjunction[Formula])
  /** The disjunction of two formulas */
  case Disjunction(disjunction: symbol.Disjunction[Formula])
  /** The implication from a formula to a formula */
  case Implication(implication: symbol.Implication[Formula])
