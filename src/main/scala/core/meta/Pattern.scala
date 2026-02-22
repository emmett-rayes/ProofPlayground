package proofPlayground
package core.meta

import scala.language.implicitConversions

import core.{Algebra, Fix, Functor, catamorphism, fix}
import core.meta.PatternF.*

type MetaVariable = PatternF.Meta[?, ?]

/** Pattern for matching formulas in proof structures.
  *
  * A pattern is defined as a fixed-point over the functor [[PatternF]].
  */
type Pattern[F[_]] = Fix[[T] =>> PatternF[F, T]]

object Pattern {

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

  /** [[MetaVars]] instance for [[Pattern]]. */
  given [F[_]: Functor] => (Algebra[F, Set[MetaVariable]]) => Pattern[F] is MetaVars {
    extension (pattern: Pattern[F]) {
      override def metavariables: Set[MetaVariable] = {
        val algebra = PatternF.algebra[Set[MetaVariable], F](summon) {
          case pattern @ PatternF.Meta(_)                            => Set(pattern)
          case PatternF.Substitution(variable, replacement, formula) => variable ++ replacement ++ formula
        }
        catamorphism(pattern)(algebra)
      }
    }
  }

  /** [[Unify]] instance for Identity.
    * The fact that this instance if for Identity is technical. It is effectively an instance for [[Pattern]]. */
  given [T, F[_] : Functor] =>(Algebra[F, MapUnifier[T]]) =>([X] =>> X) is Unify[T, F] {
    override type Unification = MapUnification

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] =
        MapUnification.merge(unification, aux)

    /** Attempt to unify a pattern with a concrete formula.
      *
      * The result is a mapping from meta-variable appearing in the pattern to concrete values when a match exists.
      *
      * Rules:
      * - Meta variables in the pattern match any formula and are bound to it.
      * - Concrete constructors must structurally match and recursively unify.
      * - Leaf cases (e.g. Variable, True, False) succeed only if equal; otherwise fail.
      * - For unary connectives (e.g. ¬), the argument must unify.
      * - For binary connectives (e.g. ∧, ∨, →), both sides must unify and their substitutions must be consistent.
      *
      * @return Some(unification) if a consistent unification exists; None otherwise
      */
    extension (pattern: Pattern[F])
      override def unifier: Unifier =
        val algebra = PatternF.algebra(summon) {
          case PatternF.Meta(name) => scrutinee => Some(Map(meta(name) -> scrutinee))
          case PatternF.Substitution(_, _, _) => _ => Some(Map.empty)
        }
        catamorphism(pattern)(algebra)
  }

  /** [[SubstitutePartial]] instance for Identity.
    * The fact that this instance if for Identity is technical. It is effectively an instance for [[Pattern]]. */
  given [T: AsPattern[F], F[_]: Functor]
    => (Algebra[F, MapUnifier[T]]) 
      => ([X] =>> X) is SubstitutePartial[T, F] {
    override type Unification = PatternUnify.Unification
    private val PatternUnify = Pattern.given_is_X_Unify

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] =
        PatternUnify.merge(unification)(aux)

    extension (pattern: Pattern[F])
      override def unifier: Unifier =
        PatternUnify.unifier(pattern)

    extension (pattern: Pattern[F])
      override def substitutePartial(unification: Unification[T]): Pattern[F] =
        def substitute(pattern: Pattern[F], unification: MapUnification[Pattern[F]]): Pattern[F] =
            pattern.unfix match {
              case pattern @ PatternF.Meta(_) =>
                unification.getOrElse(pattern, pattern)
              case PatternF.Substitution(variable, replacement, formula) =>
                substitution(
                  substitute(variable, unification),
                  substitute(replacement, unification),
                  substitute(formula, unification),
                )
              case PatternF.Formula(formula) =>
                concrete(formula.map(substitute(_, unification)))
            }
        val patternUnification = unification.view.mapValues(_.asPattern).toMap
        substitute(pattern, patternUnification)
  }

  /** [[Substitute]] instance for Identity.
    * The fact that this instance if for Identity is technical. It is effectively an instance for [[Pattern]]. */
  given [T: {AsPattern[F], CaptureAvoidingSub}, F[_] : Functor]
    => (Algebra[F, Option[T]])
    => (Algebra[F, MapUnifier[T]])
      => ([X] =>> X) is Substitute[T, F] {
    override type Unification = PatternSubstitutePartial.Unification
    private val PatternSubstitutePartial = Pattern.given_is_X_SubstitutePartial

    extension (unification: Unification[T])
      override def merge(aux: MapUnification[T]): Option[Unification[T]] =
        PatternSubstitutePartial.merge(unification)(aux)

    extension (pattern: Pattern[F])
      override def unifier: Unifier =
        PatternSubstitutePartial.unifier(pattern)
        
    extension (pattern: Pattern[F])
      override def substitutePartial(unification: Unification[T]): Pattern[F] =
        PatternSubstitutePartial.substitutePartial(pattern)(unification)

    extension (pattern: Pattern[F])
      override def substitute(unification: Unification[T]): Option[T] =
        val algebra = PatternF.algebra[Option[T], F](summon) {
          case pattern@PatternF.Meta(_) =>
            unification.get(pattern)
          case PatternF.Substitution(variable, replacement, formula) =>
            for
              variable <- variable
              replacement <- replacement
              formula <- formula
            yield formula.substituteWithoutCapturing(variable, replacement)
        }
        catamorphism(pattern)(algebra)
  }
}

/** The functor representing patterns.
  *
  * A pattern can be either a meta-variable that matches any formula
  * or a concrete formula. It is used in matching formulas in proof structures.
  *
  * @tparam F the formula functor of the referenced formulas.
  * @tparam T the type used for recursive positions.
  */
enum PatternF[F[_], T] {

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
}

case object PatternF {

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
  ): PatternF[F, T] => T = {
    case pattern @ PatternF.Meta(_)               => baseAlgebra(pattern)
    case pattern @ PatternF.Substitution(_, _, _) => baseAlgebra(pattern)
    case PatternF.Formula(formula)                => subalgebra(formula)
  }

  /** [[Functor]] instance for [[PatternF]]. */
  given [F[_]: Functor as F] => Functor[[T] =>> PatternF[F, T]] {
    extension [A](fa: PatternF[F, A]) {
      override def map[B](f: A => B): PatternF[F, B] =
        fa match {
          case Meta(name)                                   => meta(name)
          case Substitution(variable, replacement, formula) => Substitution(f(variable), f(replacement), f(formula))
          case Formula(formula)                             => concrete(F.map(formula)(f))
        }
    }
  }
}
