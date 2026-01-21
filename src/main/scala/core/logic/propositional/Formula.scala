package proofPlayground
package core.logic.propositional

import core.logic.symbol
import core.{Fix, Functor}

import scala.language.implicitConversions

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
  def apply(formula: FormulaF[Formula]): Formula = Fix(formula)

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
  /** Create a propositional variable formula using a variable identifier. */
  def variable[T](id: String): FormulaF.Variable[T] = Variable(symbol.Variable[Propositional](id))

  /** Create a propositional variable formula using the same variable. */
  def variable[T, K](varSymbol: symbol.Variable[Propositional]): FormulaF.Variable[T] = Variable(varSymbol)

  /** Create a true formula. */
  def tru[T]: FormulaF.True[T] = True(symbol.True())

  /** Create a false formula. */
  def fls[T]: FormulaF.False[T] = False(symbol.False())

  /** Marker trait for propositional logic variables. */
  sealed trait Propositional

  /** Extension methods for formulas.
    *
    * Provides DSL for constructing propositional formulas.
    */
  extension [T](t: T)(using Conversion[FormulaF[T], T])
    /** Negation operator. */
    def unary_~ : T = Negation(symbol.Negation(t))

    /** Conjunction operator. */
    def /\(other: T): T = Conjunction(symbol.Conjunction(t, other))

    /** Disjunction operator. */
    def \/(other: T): T = Disjunction(symbol.Disjunction(t, other))

    /** Implication operator. */
    def -->(other: T): T = Implication(symbol.Implication(t, other))

  /** [[Functor]] instance for [[FormulaF]]. */
  given Functor[FormulaF]:
    extension [A](fa: FormulaF[A])
      override def map[B](f: A => B): FormulaF[B] =
        fa match
          case Variable(sym)            => Variable(sym)
          case True(_)                  => True(symbol.True())
          case False(_)                 => False(symbol.False())
          case Negation(negation)       => Negation(symbol.Negation(f(negation.arg)))
          case Conjunction(conjunction) => Conjunction(symbol.Conjunction(f(conjunction.lhs), f(conjunction.rhs)))
          case Disjunction(disjunction) => Disjunction(symbol.Disjunction(f(disjunction.lhs), f(disjunction.rhs)))
          case Implication(implication) => Implication(symbol.Implication(f(implication.lhs), f(implication.rhs)))
