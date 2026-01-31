package proofPlayground
package core.meta

import core.Traverse.traverse
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.{Algebra, catamorphism}

/** A typeclass for substituting concrete formulas for meta-variables within a pattern. */
trait Substitute:
  /** The type of the concrete formula used in substitution. */
  type Self

  /** The type of the formula functor used in the pattern. */
  type Functor[_]

  extension (pattern: Pattern[Functor])
    /** Substitutes meta-variables in the pattern according to the provided unification.
      *
      * @param pattern     The pattern in which to perform the substitution.
      * @param unification The unification mapping meta-variables to concrete formulas.
      * @return Some(substituted) if the substitution is successful; None otherwise.
      */
    // noinspection ScalaDocUnknownParameter
    def substitute(unification: Unification[Self]): Option[Self]

object Substitute:
  /** Substitution algebra for the [[PatternF]] functor with carrier `Option[T]`. */
  private def algebra[
    F[_],
    T
  ](subalgebra: Algebra[F, Option[T]])(unification: Unification[T])(pattern: PatternF[F, Option[T]])
    : Option[T] =
    pattern match
      case pattern @ PatternF.Meta(name) => unification.get(pattern)
      case PatternF.Formula(formula)     => subalgebra(formula)

  /** Substitution algebra for the [[FormulaF]] functor with carrier `Option[Formula]`. */
  private def algebra[T](unification: Unification[Formula])(formula: FormulaF[Option[Formula]]): Option[Formula] =
    formula match
      case FormulaF.Variable(symbol)         => Some(variable(symbol))
      case FormulaF.True(_)                  => Some(tru)
      case FormulaF.False(_)                 => Some(fls)
      case FormulaF.Negation(negation)       => negation.arg.map(arg => ~arg)
      case FormulaF.Conjunction(conjunction) => for lhs <- conjunction.lhs; rhs <- conjunction.rhs yield lhs /\ rhs
      case FormulaF.Disjunction(disjunction) => for lhs <- disjunction.lhs; rhs <- disjunction.rhs yield lhs \/ rhs
      case FormulaF.Implication(implication) => for lhs <- implication.lhs; rhs <- implication.rhs yield lhs --> rhs

  /** [[Substitute]] instance for [[Formula]]. */
  given Formula is Substitute:
    override type Functor = FormulaF
    extension (pattern: Pattern[FormulaF])
      override def substitute(unification: Unification[Formula]): Option[Formula] =
        val subalgebra = algebra(unification)
        catamorphism(pattern)(algebra(subalgebra)(unification))

  extension [F[_], T: {Substitute { type Functor = F }, AsFormula { type Functor = F }}](patterns: Seq[Pattern[F]])
    /** Substitutes meta-variables in the sequence of patterns according to the provided unification.
      *
      * The substitution is performed in the order of the patterns.
      * The meta-variables are replaced by a sequence of concrete formulas.
      *
      * @tparam F The type of the formula functor used in the pattern.
      * @tparam T The type of the concrete formula used in substitution.
      * @param patterns    The sequence of patterns in which to perform the substitution.
      * @param unification The unification mapping meta-variables to concrete formulas.
      * @return Some(substituted) if the substitution is successful; None otherwise.
      */
    // noinspection ScalaDocUnknownParameter
    def substitute(unification: Unification[Seq[T]]): Option[Seq[T]] =
      patterns.traverse { pattern =>
        pattern.unfix match
          case pattern @ PatternF.Meta(name) => Some(unification(pattern))
          case PatternF.Formula(formula)     => pattern.asFormula.map(Seq(_))
      }.map(_.flatten)
