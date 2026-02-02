package proofPlayground
package core.meta

import core.{Algebra, Functor, Sequence, catamorphism, traverse}

object Substitute:
  type SequenceOption = [X[_]] =>> Sequence[X, Option]

  /** Substitutes meta-variables in the sequence of patterns according to the provided unification.
    *
    * The substitution is performed in the order of the patterns.
    * The meta-variables are replaced by a sequence of concrete formulas.
    *
    * @tparam T The type of the concrete formula used in substitution.
    * @tparam F The type of the formula functor used in the pattern.
    * @param patterns    The sequence of patterns in which to perform the substitution.
    * @param unification The unification mapping meta-variables to concrete formulas.
    * @return Some(substituted) if the substitution is successful; None otherwise.
    */
  def substitute[T, F[_]: {Functor, SequenceOption}](using
    Algebra[F, T]
  )(patterns: Seq[Pattern[F]], unification: Unification[Seq[T]]): Option[Seq[T]] =
    patterns.traverse { pattern =>
      pattern.unfix match
        case pattern @ PatternF.Meta(name) => unification.get(pattern)
        case PatternF.Formula(formula)     => substitute[T, F](pattern, Map.empty).map(Seq(_))
      // substituting with the empty unification converts a pattern without variables to a formula
    }.map(_.flatten)

  /** Substitutes meta-variables in the pattern according to the provided unification.
    *
    * @tparam T The type of the concrete formula used in substitution.
    * @tparam F The type of the formula functor used in the pattern.
    * @param pattern     The pattern in which to perform the substitution.
    * @param unification The unification mapping meta-variables to concrete formulas.
    * @return Some(substituted) if the substitution is successful; None otherwise.
    */
  def substitute[T, F[_]: {Functor, SequenceOption}](using
    Algebra[F, T]
  )(pattern: Pattern[F], unification: Unification[T]): Option[T] =
    catamorphism(pattern)(algebra(unification))

  /** Substitution algebra for the [[PatternF]] functor with carrier `Option[T]`. */
  private def algebra[T, F[_]: {Functor, SequenceOption}](
    using subalgebra: Algebra[F, T]
  )(unification: Unification[T])(pattern: PatternF[F, Option[T]])
    : Option[T] =
    pattern match
      case pattern @ PatternF.Meta(name) => unification.get(pattern)
      case PatternF.Formula(formula)     => formula.sequence.map(subalgebra)
