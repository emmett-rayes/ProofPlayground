package proofPlayground
package frontend.presentation

import core.logic.propositional.{Formula, FormulaF}
import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.given
import core.meta.Unifier.given
import core.meta.{MetaVariable, Unification}
import core.proof.Assistant.ProofResult
import core.proof.ProofZipper.given
import core.meta.Pattern.given
import core.proof.natural.Judgement
import core.proof.{Assistant, InferenceRule, Proof, ProofSystem}
import frontend.Show.given
import frontend.tui.Navigation
import tree.Tree
import tree.Zipper.root

import java.util
import scala.compiletime.uninitialized

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
}

class ProofTreeModel(navigation: Navigation)(formula: Formula) extends ProofTreeModel.Data, ProofTreeModel.Signals {
  private val proofSystem    = ProofSystem.IntuitionisticPropositionalNaturalDeduction
  private val inferenceRules = proofSystem.rules.toVector.sortBy(_.label)

  // hack to remember the label of the proof step for each judgement
  // uses `IdentityHashMap` which compares keys by reference equality (eq) instead of structural equality
  private val proofStepLabels = util.IdentityHashMap[Judgement[Formula], String]()

  private var selected: ProofTreeModel.ProofStep = uninitialized

  private var rulesInFocus = false
  private var zipper       = Proof(Judgement(formula, Seq.empty, Seq.empty), List.empty).zipper

  override def focusOnRules: Boolean = rulesInFocus

  override def rules: Vector[ProofTreeModel.ProofRule] = inferenceRules.map { rule =>
    val active = Assistant.proof(zipper.get.conclusion, rule) match {
      case ProofResult.UnificationFailure()    => false
      case ProofResult.SideConditionFailure(_) => false
      case ProofResult.Success(_)              => true
      case ProofResult.SubstitutionFailure(_)  => true // substitution failures can be fixed by user input
    }
    ProofTreeModel.ProofRule(active, rule.label)
  }

  override def proofTree: Tree[ProofTreeModel.ProofStep] =
    zipper.root.get.asTree.map { judgement =>
      val label =
        if judgement.free.contains(judgement.assertion) then "!"
        else if judgement.assumptions.contains(judgement.assertion) then " "
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

  override def down(): Unit =
    zipper = zipper.up.getOrElse(zipper) // proof trees are upside down

  override def left(): Unit =
    zipper = zipper.left.getOrElse(zipper)

  override def right(): Unit =
    zipper = zipper.right.getOrElse(zipper)

  override def selectNode(): Unit =
    rulesInFocus = true

  override def selectRule(index: Option[Int]): Unit = {
    if index.isEmpty then rulesInFocus = false
    for
      idx  <- index
      rule <- inferenceRules.lift(idx)
    yield applyRule(rule) { substitutedRule =>
      handleMissingMetaVariables(substitutedRule, substitutedRule.metavariables.toSeq)(Map.empty) { unification =>
        applyRule(substitutedRule, unification) { _ =>
          navigation.showPopup(Navigation.Popup.Confirm(
            "The supplied meta-variable substitutions do not match the selected rule.",
            Some("Substitution Error"),
          ))(None)
        }
      }
    }
  }

  override def quit(): Unit =
    navigation.showPopup(Navigation.Popup.Confirm("Do you want to quit the proof mode?", Some("Quit")))(Some(() =>
      navigation.navigateTo(Navigation.Screen.FormulaInput)
    ))

  private def handleMissingMetaVariables(
    rule: InferenceRule[Judgement, FormulaF],
    metavariables: Seq[MetaVariable]
  )(unification: Unification[Formula])(
    callback: Unification[Formula] => Unit
  ): Unit =
    if metavariables.isEmpty then
      callback(unification)
    else
      navigation.showPopup(Navigation.Popup.MissingMetaVariable(metavariables.head, rule)) {
        formula =>
          val updated = unification.updated(metavariables.head, formula)
          handleMissingMetaVariables(rule, metavariables.tail)(updated)(callback)
      }

  private def applyRule(
    rule: InferenceRule[Judgement, FormulaF],
    unification: Unification[Formula] = Map.empty,
  )(substitutionFailure: InferenceRule[Judgement, FormulaF] => Unit): Unit = {
    def replace(replacement: Proof[Judgement[Formula]]): Unit = {
      rulesInFocus = false
      zipper = zipper.replace(replacement)
      proofStepLabels.put(zipper.get.conclusion, rule.label)
      zipper = zipper.down.getOrElse(zipper)
    }

    Assistant.proof(zipper.get.conclusion, rule, unification) match {
      case ProofResult.UnificationFailure()                 => ()
      case ProofResult.SideConditionFailure(_)              => ()
      case ProofResult.Success(proof)                       => replace(proof)
      case ProofResult.SubstitutionFailure(substitutedRule) => substitutionFailure(substitutedRule)
    }
  }
}
