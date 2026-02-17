package proofPlayground
package core.meta

import core.{Algebra, Fix, Functor, fix}

type MetaVariable = PatternF.Meta[?, ?]

/** Pattern for matching formulas in proof structures.
  *
  * A pattern is defined as a fixed-point over the functor [[PatternF]].
  */
type Pattern[F[_]] = Fix[[T] =>> PatternF[F, T]]

object Pattern:
  /** Implicit conversion from a pattern functor to a pattern.
    *
    * Enables using a [[PatternF]] directly where a [[Pattern]] is expected.
    *
    * @tparam F the formula functor of the referenced formulas in the pattern.
    * @return a conversion that constructs a `Pattern[F]`.
    */
  given [F[_]] => Conversion[PatternF[F, Pattern[F]], Pattern[F]] = Pattern(_)

  /** Construct a pattern from its functor representation. */
  def apply[F[_]](pattern: PatternF[F, Pattern[F]]): Pattern[F] = pattern.fix

  /** Construct an algebra for a pattern functor using a subalgebra for the contained formula functor.
    *
    * @tparam T the carrier type of the algebra.
    * @tparam F the formula functor of the referenced formulas in the pattern.
    * @param baseAlgebra a function that maps a meta-variable or a substitution pattern to a value of type `T`.
    * @param subalgebra the subalgebra for the contained formula functor.
    * @return an algebra for a pattern functor that maps patterns to values of type `T`.
    */
  def algebra[T, F[_]: Functor](subalgebra: Algebra[F, T])(
    baseAlgebra: PatternF.Meta[F, T] | PatternF.Substitution[F, T] => T
  )
    : PatternF[F, T] => T = {
    case pattern @ PatternF.Meta(_)               => baseAlgebra(pattern)
    case pattern @ PatternF.Substitution(_, _, _) => baseAlgebra(pattern)
    case PatternF.Formula(formula)                => subalgebra(formula)
  }

/** The functor representing patterns.
  *
  * A pattern can be either a meta-variable that matches any formula
  * or a concrete formula. It is used in matching formulas in proof structures.
  *
  * @tparam F the formula functor of the referenced formulas.
  * @tparam T the type used for recursive positions.
  */
enum PatternF[F[_], T]:
  /** A meta-variable pattern that matches any formula.
    *
    * @param name the identifier for this meta-variable.
    */
  case Meta(name: String)

  /** A pattern that stands for the capture avoiding substitution of a concrete variable in a formula.
    *
    * @note variables are modelled by the same type as formulas. This is both for ease of development
    *       and future-support for pattern-based substitutions of arbitrary subformulas.
    *
    * @param variable the variable to be substituted.
    * @param replacement the variable to substitute with.
    * @param formula the formula in which the substitution takes place.
    */
  case Substitution(variable: T, replacement: T, formula: T)

  /** A pattern that matches a concrete formula.
    *
    * @param formula the concrete formula to match.
    */
  case Formula(formula: F[T])

case object PatternF:
  /** Implicit conversion from a meta-variable name to a meta-variable pattern. */
  given [F[_], T] => Conversion[String, PatternF[F, T]] = meta(_)

  /** Creates a meta-variable pattern with a given name. */
  def meta[F[_], T](name: String): PatternF.Meta[F, T] = PatternF.Meta(name)

  /** Implicit conversion from a concrete formula to a formula pattern. */
  given [F[_], T] => Conversion[F[T], PatternF.Formula[F, T]] = concrete(_)

  /** Creates a formula pattern from a concrete formula. */
  def concrete[F[_], T](formula: F[T]): PatternF.Formula[F, T] = PatternF.Formula(formula)

  /** Creates a substitution pattern from a variable, a replacement formula, and a formula. */
  def substitution[F[_], T](variable: T, replacement: T, formula: T): PatternF.Substitution[F, T] =
    PatternF.Substitution(variable, replacement, formula)

  /** [[Functor]] instance for [[PatternF]]. */
  given [F[_]: Functor as F] => Functor[[T] =>> PatternF[F, T]]:
    extension [A](fa: PatternF[F, A])
      override def map[B](f: A => B): PatternF[F, B] =
        fa match
          case Meta(name)                                   => meta(name)
          case Substitution(variable, replacement, formula) => Substitution(f(variable), f(replacement), f(formula))
          case Formula(formula)                             => concrete(F.map(formula)(f))
