//noinspection DuplicatedCode
package proofPlayground
package core.system.natural

import core.logic.propositional.FormulaF
import core.logic.symbol
import core.meta.Pattern.Formula
import core.meta.{Inference, Pattern}

/** An inference rule for natural deduction judgements.
 *
 * Inference rules contain patterns of judgements.
 */
type InferenceRule[F[_]] = Inference[Judgement[Pattern.Seq, Pattern.Formula[F]]]

/** Collection of inference rules for natural deduction.
 */
case object InferenceRules:

  /** Inference rules for intuitionistic propositional logic.
   */
  case object IntuitionisticPropositional:

    /** Conjunction introduction (∧I).
     *
     * If Γ ⊢ A and Γ ⊢ B, then Γ ⊢ A ∧ B.
     */
    val ConjunctionIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")

      Inference(
        Set(Judgement(gamma, a), Judgement(gamma, b)),
        Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Conjunction(symbol.Conjunction(a, b)))),
      )

    /** Conjunction elimination 1 (∧E₁).
     *
     * If Γ ⊢ A ∧ B, then Γ ⊢ A.
     */
    val ConjunctionElimination1: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")

      Inference(
        Set(Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Conjunction(symbol.Conjunction(a, b))))),
        Judgement(gamma, a),
      )

    /** Conjunction elimination 2 (∧E₂).
     *
     * If Γ ⊢ A ∧ B, then Γ ⊢ B.
     */
    val ConjunctionElimination2: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")

      Inference(
        Set(Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Conjunction(symbol.Conjunction(a, b))))),
        Judgement(gamma, b),
      )

    /** Disjunction introduction 1 (∨I₁).
     *
     * If Γ ⊢ A, then Γ ⊢ A ∨ B.
     */
    val DisjunctionIntroduction1: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")

      Inference(
        Set(Judgement(gamma, a)),
        Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Disjunction(symbol.Disjunction(a, b)))),
      )

    /** Disjunction introduction 2 (∨I₂).
     *
     * If Γ ⊢ B, then Γ ⊢ A ∨ B.
     */
    val DisjunctionIntroduction2: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")

      Inference(
        Set(Judgement(gamma, b)),
        Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Disjunction(symbol.Disjunction(a, b)))),
      )

    /** Disjunction elimination (∨E).
     *
     * If Γ ⊢ A ∨ B, Γ,A ⊢ C and Γ,B ⊢ C, then Γ ⊢ C.
     */
    val DisjunctionElimination: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")
      val c = MetaVariable("C")

      Inference(
        Set(
          Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Disjunction(symbol.Disjunction(a, b)))),
          Judgement(Pattern.Seq.Concrete(scala.Seq(gamma, a)), c),
          Judgement(Pattern.Seq.Concrete(scala.Seq(gamma, b)), c),
        ),
        Judgement(gamma, c)
      )

    /** Implication introduction (→I).
     *
     * If Γ,A ⊢ B, then Γ ⊢ A → B.
     */
    val ImplicationIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")

      Inference(
        Set(Judgement(Pattern.Seq.Concrete(scala.Seq(gamma, a)), b)),
        Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Implication(symbol.Implication(a, b)))),
      )

    /** Implication elimination (→E).
     *
     * If Γ ⊢ A → B and Γ ⊢ A, then Γ ⊢ B.
     */
    val ImplicationElimination: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")
      val b = MetaVariable("B")

      Inference(
        Set(
          Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Implication(symbol.Implication(a, b)))),
          Judgement(gamma, a),
        ),
        Judgement(gamma, b)
      )

    /** Negation introduction (¬I).
     *
     * If Γ,A ⊢ ⊥, then Γ ⊢ ¬A.
     */
    val NegationIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")

      Inference(
        Set(Judgement(Pattern.Seq.Concrete(scala.Seq(gamma, a)), Pattern.Formula.Concrete(FormulaF.False(symbol.False())))),
        Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Negation(symbol.Negation(a)))),
      )

    /** Negation elimination (¬E).
     *
     * If Γ ⊢ ¬A and Γ ⊢ A, then Γ ⊢ ⊥.
     */
    val NegationElimination: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")

      Inference(
        Set(
          Judgement(gamma, Pattern.Formula.Concrete(FormulaF.Negation(symbol.Negation(a)))),
          Judgement(gamma, a),
        ),
        Judgement(gamma, Pattern.Formula.Concrete(FormulaF.False(symbol.False()))),
      )

    /** True introduction (⊤I).
     *
     * Principle of implosion (ex quodlibet verum).
     *
     * Γ ⊢ ⊤.
     */
    val TrueIntroduction: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")

      Inference(
        Set(),
        Judgement(gamma, Pattern.Formula.Concrete(FormulaF.True(symbol.True()))),
      )

    /** False elimination (⊥E).
     *
     * Principle of explosion (ex falso quodlibet).
     *
     * If Γ ⊢ ⊥, then Γ ⊢ A.
     */
    val FalseElimination: InferenceRule[FormulaF] =
      val gamma = Pattern.Seq.Meta("Gamma")
      val a = MetaVariable("A")

      Inference(
        Set(Judgement(gamma, Pattern.Formula.Concrete(FormulaF.False(symbol.False())))),
        Judgement(gamma, a),
      )

    private object MetaVariable:
      /** Helper for creating formula meta-variables.
       *
       * @param str The meta-variable name.
       * @return A `Pattern.Formula.Meta[FormulaF]` instance.
       */
      def apply(str: String) = Pattern.Formula.Meta[FormulaF](str)
