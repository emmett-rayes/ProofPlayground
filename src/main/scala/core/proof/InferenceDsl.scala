package proofPlayground
package core.proof

import scala.language.implicitConversions

import core.meta.Pattern
import core.meta.Pattern.given
import core.meta.PatternF.*

object InferenceDsl {

  /** Implicit conversion from a formula over patterns to a pattern. */
  given [F[_]] => Conversion[F[Pattern[F]], Pattern[F]] = f => Pattern(concrete(f))

  /** Implicit conversion from a pattern to a singleton sequence containing that pattern. */
  given [F[_]] => Conversion[Pattern[F], Seq[Pattern[F]]] = Seq(_)

  /** An intermediate type used to collect the parts of a substitution pattern. */
  opaque type SubstitutionContext[F[_]] = (Pattern[F], Pattern[F])

  extension [F[_]](patterns: Seq[Pattern[F]]) {

    /** Add a pattern to a sequence of patterns.
      *
      * @param pattern The pattern to add to the sequence.
      * @return A new sequence containing the original patterns and the added pattern.
      */
    def ::(pattern: Pattern[F]): Seq[Pattern[F]] = patterns :+ pattern
  }

  extension [F[_]](pattern: Pattern[F]) {

    /** Create a substitution pattern context.
      *
      * @param other The pattern to substitute for the subpattern.
      * @return A new substitution context containing the subpattern and the pattern to substitute.
      */
    def /(other: Pattern[F]): SubstitutionContext[F] = (pattern, other)

    /** Create a substitution pattern.
      *
      * @param context The substitution context containing the subpattern and the pattern to substitute.
      * @return A new pattern resulting from substituting the second pattern for the first pattern in the original pattern.
      */
    def apply(context: SubstitutionContext[F]): Pattern[F] =
      substitution(context._2, context._1, pattern)
  }
}
