package proofPlayground
package core.logic.propositional

import core.logic.symbol.{Conjunction as SymConjunction, Difference as SymDifference, Disjunction as SymDisjunction, False as SymFalse, Implication as SymImplication, Negation as SymNegation, True as SymTrue, Variable as SymVariable}

/** Marker trait for propositional logic variables */
sealed trait Propositional

/** Represents propositional logic formulas */
enum Formula:
  /** A propositional variable */
  case Variable(variable: SymVariable[Propositional])
  /** The true constant */
  case True(tru: SymTrue)
  /** The false constant */
  case False(fls: SymFalse)
  /** The negation of a formula */
  case Negation(negation: SymNegation[Formula])
  /** The conjunction of two formulas */
  case Conjunction(conjunction: SymConjunction[Formula])
  /** The disjunction of two formulas */
  case Disjunction(disjunction: SymDisjunction[Formula])
  /** The implication from a formula to a formula */
  case Implication(implication: SymImplication[Formula])
  /** The difference of a formula from a formula */
  case Difference(difference: SymDifference[Formula])
