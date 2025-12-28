package proofPlayground
package core.logic.propositional

import core.logic.symbol

/** Representation of a propositional formula.
 *
 * A formula is defined as a fixed point over the functor FormulaF
 */
case class Formula(formula: FormulaF[Formula])

/** Representation of the structure of a propositional logic formula.
 *
 * @tparam F the type used for recursive positions.
 */
enum FormulaF[F]:

  /** A propositional variable. */
  case Variable(variable: symbol.Variable[FormulaF.Propositional])

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

case object FormulaF:
  /** Marker trait for propositional logic variables. */
  sealed trait Propositional

  /** Create a propositional variable formula */
  def variable[F](): FormulaF[F] = FormulaF.Variable(symbol.Variable[Propositional]())

  /** Create a true formula. */
  def tru[F]: FormulaF[F] = FormulaF.True(symbol.True())

  /** Create a false formula. */
  def fls[F]: FormulaF[F] = FormulaF.False(symbol.False())

  /** Extension methods for formulas.
   *
   * Provides DSL for constructing propositional formulas.
   */
  extension [F](f: F)
    /** Negation operator. */
    def unary_~ : FormulaF[F] = FormulaF.Negation(symbol.Negation(f))

    /** Conjunction operator. */
    def /\(other: F): FormulaF[F] = FormulaF.Conjunction(symbol.Conjunction(f, other))

    /** Disjunction operator. */
    def \/(other: F): FormulaF[F] = FormulaF.Disjunction(symbol.Disjunction(f, other))

    /** Implication operator. */
    def -->(other: F): FormulaF[F] = FormulaF.Implication(symbol.Implication(f, other))
