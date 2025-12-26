package proofPlayground
package core.logic.symbol

/** True (⊤). */
case class True()

/** False (⊥). */
case class False()

/** Unary negation (¬).
 *
 * @tparam F The type of formula being negated.
 * @param arg The formula to negate.
 */
case class Negation[F](arg: F)

/** Binary conjunction (∧).
 *
 * @tparam F The type of formulas being combined.
 * @param lhs Left-hand side formula.
 * @param rhs Right-hand side formula.
 */
case class Conjunction[F](lhs: F, rhs: F)

/** Binary disjunction (∨).
 *
 * @tparam F The type of formulas being combined.
 * @param lhs Left-hand side formula.
 * @param rhs Right-hand side formula.
 */
case class Disjunction[F](lhs: F, rhs: F)

/** Implication (→).
 *
 * @tparam F The type of formulas being combined.
 * @param lhs Left-hand side formula (antecedent).
 * @param rhs Right-hand side formula (consequent).
 */
case class Implication[F](lhs: F, rhs: F)

/** Difference (-<).
 *
 * @tparam F The type of formulas being combined.
 * @param lhs Left-hand side formula.
 * @param rhs Right-hand side formula.
 */
case class Difference[F](lhs: F, rhs: F)
