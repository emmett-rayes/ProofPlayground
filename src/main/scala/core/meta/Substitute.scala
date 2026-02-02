package proofPlayground
package core.meta

import core.{Algebra, Functor, catamorphism, traverse}
import core.logic.propositional.{Formula, FormulaF}

object Substitute:
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
  def substitute[T, F[_]: Functor](using
    Algebra[F, Option[T]]
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
  def substitute[T, F[_]: Functor](using
    Algebra[F, Option[T]]
  )(pattern: Pattern[F], unification: Unification[T]): Option[T] =
    val algebra = Pattern.algebra[Option[T], F](summon)(unification.get(_))
    catamorphism(pattern)(algebra)

  /** An algebra for collapsing a [[Formula]] into an `Option[Formula]`. */
  given Algebra[FormulaF, Option[Formula]]:
    import core.logic.propositional.Formula.given
    import core.logic.propositional.FormulaF.*

    override def apply(formula: FormulaF[Option[Formula]]): Option[Formula] = {
      formula match
        case FormulaF.Variable(sym)            => Some(variable(sym))
        case FormulaF.True(_)                  => Some(tru)
        case FormulaF.False(_)                 => Some(fls)
        case FormulaF.Negation(negation)       => negation.arg.map(arg => ~arg)
        case FormulaF.Conjunction(conjunction) => for lhs <- conjunction.lhs; rhs <- conjunction.rhs yield lhs /\ rhs
        case FormulaF.Disjunction(disjunction) => for lhs <- disjunction.lhs; rhs <- disjunction.rhs yield lhs \/ rhs
        case FormulaF.Implication(implication) => for lhs <- implication.lhs; rhs <- implication.rhs yield lhs --> rhs
    }
