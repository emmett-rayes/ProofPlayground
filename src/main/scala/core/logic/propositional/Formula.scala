package proofPlayground
package core.logic.propositional

import core.catamorphism
import core.logic.propositional.FormulaF.tru
import core.logic.symbol
import core.meta.{AsPattern, Pattern}
import core.meta.PatternF.*
import core.logic.propositional.FormulaF.*
import core.{Algebra, Fix, Functor, fix}

import scala.language.implicitConversions

/** Representation of a propositional formula.
  *
  * A formula is defined as a fixed-point over the functor [[FormulaF]].
  */
type Formula = Fix[FormulaF]

object Formula:
  /** Implicit conversion from a formula functor to a formula.
    *
    * Enables using a [[FormulaF]] directly where a [[Formula]] is expected.
    */
  given Conversion[FormulaF[Formula], Formula] = Formula(_)

  /** Construct a formula from its functor representation. */
  def apply(formula: FormulaF[Formula]): Formula = formula.fix

  /** [[Fix]] is the initial algebra for [[FormulaF]]. */
  given Algebra[FormulaF, Fix[FormulaF]] = Fix(_)

  /** [[AsPattern]] instance for [[Formula]]. */
  given Formula is AsPattern[FormulaF]:
    private given Conversion[FormulaF[Pattern[FormulaF]], Pattern[FormulaF]] = concrete(_).fix

    private def algebra: Algebra[FormulaF, Pattern[FormulaF]] = {
      case FormulaF.Variable(symbol)         => variable(symbol)
      case FormulaF.True(_)                  => tru
      case FormulaF.False(_)                 => fls
      case FormulaF.Negation(negation)       => ~negation.arg
      case FormulaF.Conjunction(conjunction) => conjunction.lhs /\ conjunction.rhs
      case FormulaF.Disjunction(disjunction) => disjunction.lhs \/ disjunction.rhs
      case FormulaF.Implication(implication) => implication.lhs --> implication.rhs
      case FormulaF.Universal(universal)     => forall(universal.variable, universal.body)
      case FormulaF.Existential(existential) => exists(existential.variable, existential.body)
    }

    extension (formula: Formula)
      override def asPattern: Pattern[FormulaF] =
        catamorphism(formula)(algebra)

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

  /** The universal abstraction of a formula. */
  case Universal(universal: symbol.Universal[T, T])

  /** The existential abstraction of a formula. */
  case Existential(existential: symbol.Existential[T, T])

case object FormulaF:
  /** Create a propositional variable formula using a variable identifier. */
  def variable[T](id: String)(using Conversion[FormulaF[T], T]): T = Variable(symbol.Variable[Propositional](id))

  /** Create a propositional variable formula using the same variable. */
  def variable[T](varSymbol: symbol.Variable[Propositional])(using Conversion[FormulaF[T], T]): T = Variable(varSymbol)

  /** Create a true formula. */
  def tru[T](using Conversion[FormulaF[T], T]): T = True(symbol.True())

  /** Create a false formula. */
  def fls[T](using Conversion[FormulaF[T], T]): T = False(symbol.False())

  /** Create a universally quantified formula. */
  def forall[T](using Conversion[FormulaF[T], T])(variable: T, formula: T): T =
    Universal(symbol.Universal(variable, formula))

  /** Create an existentially quantified formula. */
  def exists[T](using Conversion[FormulaF[T], T])(variable: T, formula: T): T =
    Existential(symbol.Existential(variable, formula))

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
          case Universal(universal)     => Universal(symbol.Universal(f(universal.variable), f(universal.body)))
          case Existential(existential) => Existential(symbol.Existential(f(existential.variable), f(existential.body)))
