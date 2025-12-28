package proofPlayground
package core.logic.propositional

import core.logic.symbol
import core.{Fix, Functor}

/** Representation of a propositional formula.
 *
 * A formula is defined as a fixed point over the functor [[FormulaF]].
 */
type Formula = Fix[FormulaF]

object Formula:
  /** Implicit conversion from a formula functor to a formula.
   *
   * Enables using a [[FormulaF]] directly where a [[Formula]] is expected.
   *
   * @return a conversion that constructs a `Formula`.
   */
  given Conversion[FormulaF[Formula], Formula] = Formula(_)

  /** Construct a formula from its functor representation. */
  def apply(f: FormulaF[Formula]): Formula = Fix(f)


/** The functor representing the structure of a propositional logic formula.
 *
 * @tparam T the type used for recursive positions.
 */
enum FormulaF[T]:
  /** A propositional variable. */
  case Variable(variable: symbol.Variable[FormulaF.Propositional])

  /** The true constant. */
  case True(tru: symbol.True)

  /** The false constant. */
  case False(fls: symbol.False)

  /** The negation of a formula. */
  case Negation(negation: symbol.Negation[T])

  /** The conjunction of two formulas. */
  case Conjunction(conjunction: symbol.Conjunction[T])

  /** The disjunction of two formulas. */
  case Disjunction(disjunction: symbol.Disjunction[T])

  /** The implication from a formula to a formula. */
  case Implication(implication: symbol.Implication[T])

case object FormulaF:
  /** Marker trait for propositional logic variables. */
  sealed trait Propositional

  /** Create a propositional variable formula using a fresh variable */
  def variable[T](): FormulaF[T] = Variable(symbol.Variable[Propositional]())

  /** Create a propositional variable formula using the same variable */
  def variable[T, K](varSymbol: symbol.Variable[Propositional]): FormulaF[T] = Variable(varSymbol)

  /** Create a true formula. */
  def tru[T]: FormulaF[T] = True(symbol.True())

  /** Create a false formula. */
  def fls[T]: FormulaF[T] = False(symbol.False())

  /** Extension methods for formulas.
   *
   * Provides DSL for constructing propositional formulas.
   */
  extension [T](t: T)
    /** Negation operator. */
    def unary_~ : FormulaF[T] = Negation(symbol.Negation(t))

    /** Conjunction operator. */
    def /\(other: T): FormulaF[T] = Conjunction(symbol.Conjunction(t, other))

    /** Disjunction operator. */
    def \/(other: T): FormulaF[T] = Disjunction(symbol.Disjunction(t, other))

    /** Implication operator. */
    def -->(other: T): FormulaF[T] = Implication(symbol.Implication(t, other))

  /** [[Functor]] instance for [[FormulaF]]. */
  given Functor[FormulaF]:
    extension [A](fa: FormulaF[A])
      override def map[B](f: A => B): FormulaF[B] =
        fa match
          case Variable(sym) => variable(sym)
          case True(_) => tru
          case False(_) => fls
          case Negation(negation) => ~f(negation.arg)
          case Conjunction(conjunction) => f(conjunction.lhs) /\ f(conjunction.rhs)
          case Disjunction(disjunction) => f(disjunction.lhs) \/ f(disjunction.rhs)
          case Implication(implication) => f(implication.lhs) --> f(implication.rhs)
