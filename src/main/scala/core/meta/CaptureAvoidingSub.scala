package proofPlayground
package core.meta

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.meta.FreeVars.given_is_Formula_FreeVars

import scala.annotation.tailrec

trait CaptureAvoidingSub:
  type Self

  extension (self: Self)
    /** Performs capture-avoiding substitution of `term` for `variable` in `self`.
      *
      * @param variable the variable to be substituted in `self`.
      * @param replacement the term to substitute for `variable` in `self`.
      * @return a new value with `term` substituted for `variable`, avoiding variable capture.
      */
    def substituteWithoutCapturing(variable: Self, replacement: Self): Self

object CaptureAvoidingSub:
  /** Instance of [[CaptureAvoidingSub]] for [[Formula]]. */
  given Formula is CaptureAvoidingSub:
    /** Rename the bound variable in the scope formula to the fresh variable.
      *
      * @param bound the variable to be renamed
      * @param scope the formula in which the variable is bound
      * @param avoid a set of formulas whose free variables should be avoided
      */
    private def alphaConversion(
      bound: Formula,
      scope: Formula,
      avoid: Set[Formula],
    ): (renamedVariable: Formula, renamedScoped: Formula) =
      val boundName = bound.unfix match {
        case FormulaF.Variable(variable) => variable.id
        case _                           =>
          throw RuntimeException("alpha conversion of non-variable formulas is not supported")
      }

      val fresh        = freshVariable(boundName, Set(bound, scope) ++ avoid)
      val renamedScope = scope.substituteWithoutCapturing(bound, fresh)
      (fresh, renamedScope)

    /** Generate a fresh variable name that doesn't appear in the given set of formulas.
      *
      * @param baseName the base name to start from
      * @param avoid the set of formulas whose free variables should be avoided
      * @return a fresh variable formula that doesn't conflict with any free variables in avoid
      */
    private def freshVariable(baseName: String, avoid: Set[Formula]): Formula =
      val usedNames = avoid.flatMap(_.freevariables).collect {
        case formula if formula.unfix.isInstanceOf[FormulaF.Variable[Formula]] =>
          formula.unfix match {
            case FormulaF.Variable(variable) => variable.id; case _ => ""
          }
      }

      @tailrec
      def findFreshName(base: String, counter: Int): String =
        val candidate = if counter == 0 then base else s"$base$counter"
        if usedNames.contains(candidate) then findFreshName(base, counter + 1)
        else candidate

      variable[Formula](findFreshName(baseName, 0))

    extension (formula: Formula)
      override def substituteWithoutCapturing(variable: Formula, replacement: Formula): Formula =
        // Use explicit recursion instead of catamorphism to avoid substituting binding variables
        def recurse(f: Formula): Formula =
          f.unfix match
            case v if v == variable.unfix          => replacement
            case FormulaF.Variable(_)              => f
            case FormulaF.True(_)                  => f
            case FormulaF.False(_)                 => f
            case FormulaF.Negation(negation)       => ~recurse(negation.arg)
            case FormulaF.Conjunction(conjunction) => recurse(conjunction.lhs) /\ recurse(conjunction.rhs)
            case FormulaF.Disjunction(disjunction) => recurse(disjunction.lhs) \/ recurse(disjunction.rhs)
            case FormulaF.Implication(implication) => recurse(implication.lhs) --> recurse(implication.rhs)
            case FormulaF.Universal(universal)     =>
              if universal.variable == variable then f
              else if replacement.freevariables.contains(universal.variable) then
                val (fresh, renamedBody) =
                  alphaConversion(universal.variable, universal.body, Set(variable, replacement))
                forall(fresh, recurse(renamedBody))
              else
                forall(universal.variable, recurse(universal.body))
            case FormulaF.Existential(existential) =>
              if existential.variable == variable then f
              else if replacement.freevariables.contains(existential.variable) then
                val (fresh, renamedBody) =
                  alphaConversion(existential.variable, existential.body, Set(variable, replacement))
                exists(fresh, recurse(renamedBody))
              else
                exists(existential.variable, recurse(existential.body))

        recurse(formula)
