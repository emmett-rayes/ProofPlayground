package proofPlayground
package core.meta

import core.{Fix, Functor}

/** Pattern for matching formulas in proof structures.
 *
 * A formula pattern is defined as a fixed point over the functor [[PatternF]].
 */
type Pattern[F[_]] = Fix[[T] =>> PatternF[F, T]]

object Pattern:
  /** Implicit conversion from a formula pattern functor to a formula pattern.
   *
   * Enables using a [[PatternF]] directly where a [[Pattern]] is expected.
   *
   * @tparam F the formula functor of the referenced formulas in the pattern.
   * @return a conversion that constructs a `Pattern[F]`.
   */
  given [F[_]] => Conversion[PatternF[F, Pattern[F]], Pattern[F]] = Pattern(_)

  /** Construct a formula pattern from its functor representation. */
  def apply[F[_]](f: PatternF[F, Pattern[F]]): Pattern[F] = Fix(f)

/** The higher-order functor representing formula patterns.
 *
 * A Formula pattern can be either a meta-variable that matches any formula
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

  /** A concrete formula pattern that matches a specific formula.
   *
   * @param formula the specific formula to match.
   */
  case Concrete(formula: F[T])


case object PatternF:
  /** Implicit conversion from a meta-variable name to a meta-variable pattern. */
  given [F[_], T] => Conversion[String, PatternF[F, T]] = meta(_)

  /** Create a meta-variable pattern with a given name. */
  def meta[F[_], T](name: String): PatternF.Meta[F, T] = PatternF.Meta(name)

  /** Implicit conversion from a concrete formula to a concrete formula pattern. */
  given [F[_], T] => Conversion[F[T], PatternF.Concrete[F, T]] = concrete(_)

  /** Create a concrete formula pattern. */
  def concrete[F[_], T](formula: F[T]): PatternF.Concrete[F, T] = PatternF.Concrete(formula)

  /** [[Functor]] instance for `[[PatternF]]`. */
  given [F[_] : Functor as F] =>Functor[[T] =>> PatternF[F, T]]:
    extension [A](fa: PatternF[F, A])
      override def map[B](f: A => B): PatternF[F, B] =
        fa match
          case Meta(name) => meta(name)
          case Concrete(formula) => concrete(F.map(formula)(f))
