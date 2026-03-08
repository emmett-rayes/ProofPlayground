package proofPlayground
package frontend.presentation

import java.util
import scala.compiletime.uninitialized

import core.Functor
import core.logic.propositional.Formula.given
import core.logic.propositional.{Formula, FormulaF}
import core.meta.{MetaVariable, MetaVars, MapUnification, Pattern, Substitute}
import core.proof.Assistant.ProofResult
import core.proof.ProofZipper.given
import core.proof.{Assistant, InferenceRule, Proof, ProofSystem, SideCondition}
import frontend.Show
import frontend.presentation.FormulaInputModel.ProofSystemChoice
import frontend.tui.Navigation
import zipper.Tree
import zipper.Zipper.root

object ProofTreeModel {
  trait Data {
    def proofTree: Tree[ProofStep]
    def rules: Vector[ProofRule]
    def focusOnRules: Boolean
    def isNodeSelected(node: Tree[ProofStep]): Boolean
  }

  trait Signals {
    def quit(): Unit
    def up(): Unit
    def down(): Unit
    def left(): Unit
    def right(): Unit
    def selectNode(): Unit
    def selectRule(index: Option[Int]): Unit
  }

  case class ProofStep(formula: String, rule: String)
  case class ProofRule(active: Boolean, rule: String)

  def apply(navigation: Navigation)(
    formula: Formula,
    system: FormulaInputModel.ProofSystemChoice
  ): ProofTreeModel[?] = {
    import core.meta.Pattern.given
    import frontend.Show.given

    system match {
      case ProofSystemChoice.IntuitionisticNaturalDeduction =>
        import core.proof.natural
        import core.proof.natural.Judgement.given
        new ProofTreeModel(navigation)(
          ProofSystem.IntuitionisticPropositionalNaturalDeduction,
          natural.Judgement(formula, Seq.empty, Seq.empty)
        )
      case ProofSystemChoice.ClassicalSequentCalculus =>
        import core.proof.sequent
        import core.proof.sequent.Judgement.given
        new ProofTreeModel(navigation)(
          ProofSystem.ClassicalPropositionalSequentCalculus,
          sequent.Judgement(Seq.empty, Seq(formula))
        )
    }
  }
}

class ProofTreeModel[
  J[_] <: AnyRef: {Functor, Substitute[Formula, FormulaF]}
](navigation: Navigation)(system: ProofSystem[J, FormulaF], judgement: J[Formula])(using
  J[Formula] is Show,
  J[Pattern[FormulaF]] is Show,
  J[Pattern[FormulaF]] is MetaVars,
  J[Formula] is SideCondition[Formula],
) extends ProofTreeModel.Data, ProofTreeModel.Signals {

  private val inferenceRules = system.rules.toVector.sortBy(_.label)

  // hack to remember the label of the proof step for each judgement
  // uses `IdentityHashMap` which compares keys by reference equality (eq) instead of structural equality
  private val proofStepLabels = util.IdentityHashMap[J[Formula], String]()

  private var selected: ProofTreeModel.ProofStep = uninitialized

  private var rulesInFocus = false
  private var zipper       = Proof(judgement, List.empty).zipper

  private var cachedRuleActive: Map[InferenceRule[J, FormulaF], Boolean] = Map.empty

  private def invalidateRuleCache(): Unit =
    cachedRuleActive = Map.empty

  override def focusOnRules: Boolean = rulesInFocus

  override def rules: Vector[ProofTreeModel.ProofRule] = inferenceRules.map { rule =>
    val active = cachedRuleActive.getOrElse(
      rule, {
        val computed = Assistant.proof(zipper.get.conclusion, rule) match {
          case ProofResult.UnificationFailure(_)  => false
          case ProofResult.Success(_)             => true
          case ProofResult.SubstitutionFailure(_) => true // substitution failures can be fixed by user input
        }
        cachedRuleActive = cachedRuleActive.updated(rule, computed)
        computed
      }
    )
    ProofTreeModel.ProofRule(active, rule.label)
  }

  override def proofTree: Tree[ProofTreeModel.ProofStep] =
    val tree   = zipper.root.get.asTree
    val leaves = tree.leaves.map(_.value).toSet
    tree.map { judgement =>
      val label =
        if judgement.violations.nonEmpty then "!"
        else if !judgement.open then " "
        else proofStepLabels.getOrDefault(judgement, "?")
      val result = ProofTreeModel.ProofStep(judgement.show, label)
      // remember the current position for `isNodeSelected`
      if judgement eq zipper.get.conclusion then selected = result
      result
    }

  override def isNodeSelected(node: Tree[ProofTreeModel.ProofStep]): Boolean =
    selected eq node.value

  override def up(): Unit =
    zipper = zipper.down.getOrElse(zipper) // proof trees are upside down
    invalidateRuleCache()

  override def down(): Unit =
    zipper = zipper.up.getOrElse(zipper) // proof trees are upside down
    invalidateRuleCache()

  override def left(): Unit =
    zipper = zipper.left.getOrElse(zipper)
    invalidateRuleCache()

  override def right(): Unit =
    zipper = zipper.right.getOrElse(zipper)
    invalidateRuleCache()

  override def selectNode(): Unit =
    rulesInFocus = true

  override def selectRule(index: Option[Int]): Unit = {
    if index.isEmpty then rulesInFocus = false
    for
      idx  <- index
      rule <- inferenceRules.lift(idx)
    yield applyRule(rule) { substitutedRule =>
      handleMissingMetaVariables(substitutedRule, substitutedRule.metavariables.toSeq) { unification =>
        applyRule(substitutedRule, unification) { _ =>
          navigation.showPopup(Navigation.Popup.Confirm(
            "The supplied meta-variable substitutions are not sufficient for applying the rule.",
            title = Some("Substitution Error"),
            hasConfirm = true,
          )) { () => () }
        }
      }
    }
  }

  override def quit(): Unit =
    navigation.showPopup(Navigation.Popup.Confirm("Do you want to quit the proof mode?", Some("Quit"))) { () =>
      navigation.navigateTo(Navigation.Screen.FormulaInput)
    }

  private def handleMissingMetaVariables(
    rule: InferenceRule[J, FormulaF],
    metavariables: Seq[MetaVariable],
    unification: J.Uni[Formula] = J.Uni.empty
  )(
    callback: J.Uni[Formula] => Unit
  ): Unit = {
    recur(metavariables, unification)

    def recur(metavariables: Seq[MetaVariable], unification: J.Uni[Formula]): Unit = {
      if metavariables.isEmpty then
        callback(unification)
      else
        navigation.showPopup(Navigation.Popup.MissingMetaVariable(metavariables.head, rule)) {
          formula =>
            val updated = unification.update(Map(metavariables.head -> formula))
            recur(metavariables.tail, updated.get)
        }
    }
  }

  private def applyRule(
    rule: InferenceRule[J, FormulaF],
    unification: J.Uni[Formula] = J.Uni.empty,
  )(substitutionFailure: InferenceRule[J, FormulaF] => Unit): Unit = {
    def replace(replacement: Proof[J[Formula]]): Unit = {
      rulesInFocus = false
      zipper = zipper.replace(replacement)
      proofStepLabels.put(zipper.get.conclusion, rule.label)
      zipper = zipper.down.getOrElse(zipper)
      invalidateRuleCache()
    }

    Assistant.proof(zipper.get.conclusion, rule, Some(unification)) match {
      case ProofResult.UnificationFailure(_)                => ()
      case ProofResult.Success(proof)                       => replace(proof)
      case ProofResult.SubstitutionFailure(substitutedRule) => substitutionFailure(substitutedRule)
    }
  }
}
