package proofPlayground
package frontend.tui.models

import core.logic.propositional.{Formula, FormulaF}
import core.meta.MetaVars.given
import core.meta.Substitute.given
import core.meta.Unify.given
import core.meta.{MetaVariable, Unification}
import core.proof.Assistant.ProofResult
import core.proof.ProofZipper.given
import core.proof.natural.Judgement
import core.proof.{Assistant, InferenceRule, Proof, ProofSystem}
import frontend.Show.given
import frontend.tui.Navigation
import tree.Tree
import tree.Zipper.root

import java.util
import scala.compiletime.uninitialized

object ProofTreeModel:
  trait Data:
    def proofTree: Tree[ProofStep]
    def rules: Vector[ProofRule]
    def focusOnRules: Boolean
    def isNodeSelected(node: Tree[ProofStep]): Boolean

  trait Signals:
    def quit(): Unit
    def up(): Unit
    def down(): Unit
    def left(): Unit
    def right(): Unit
    def selectNode(): Unit
    def selectRule(index: Option[Int]): Unit

  case class ProofStep(formula: String, rule: String)
  case class ProofRule(active: Boolean, rule: String)

class ProofTreeModel(navigation: Navigation)(formula: Formula) extends ProofTreeModel.Data, ProofTreeModel.Signals:
  private val proofSystem    = ProofSystem.IntuitionisticPropositionalNaturalDeduction
  private val inferenceRules = proofSystem.rules.toVector.sortBy(_.label)

  // hack to remember the label of the proof step for each judgement
  // uses `IdentityHashMap` which compares keys by reference equality (eq) instead of structural equality
  private val proofStepLabels = util.IdentityHashMap[Judgement[Formula], String]()

  private var selected: ProofTreeModel.ProofStep = uninitialized

  private var rulesInFocus = false
  private var zipper       = Proof(Judgement(Set.empty, formula), List.empty).zipper

  override def focusOnRules: Boolean = rulesInFocus

  override def rules: Vector[ProofTreeModel.ProofRule] = inferenceRules.map { rule =>
    val active = Assistant.proof(zipper.get.conclusion, rule) match
      case ProofResult.Success(_)             => true
      case ProofResult.SubstitutionFailure(_) => true // substitution failures can be fixed by user input
      case ProofResult.UnificationFailure()   => false
    ProofTreeModel.ProofRule(active, rule.label)
  }

  override def proofTree: Tree[ProofTreeModel.ProofStep] =
    zipper.root.get.asTree.map { judgement =>
      val label =
        if judgement.assumptions(judgement.assertion) then " " else proofStepLabels.getOrDefault(judgement, "?")
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

  override def selectRule(index: Option[Int]): Unit =
    if index.isEmpty then rulesInFocus = false
    for
      idx  <- index
      rule <- inferenceRules.lift(idx)
    yield
      def replace(replacement: Proof[Judgement[Formula]]) =
        rulesInFocus = false
        zipper = zipper.replace(replacement)
        proofStepLabels.put(zipper.get.conclusion, rule.label)

      Assistant.proof(zipper.get.conclusion, rule) match
        case ProofResult.UnificationFailure() =>
          ()
        case ProofResult.Success(proof) =>
          replace(proof)
        case ProofResult.SubstitutionFailure(partiallySubstitutedRule) =>
          val metavariables = partiallySubstitutedRule.metavariables: Set[MetaVariable]
          handleMissingMetaVariables(partiallySubstitutedRule, metavariables.toSeq)(Map.empty) { unification =>
            Assistant.proof(zipper.get.conclusion, partiallySubstitutedRule, unification) match
              case Assistant.ProofResult.UnificationFailure() =>
                ()
              case Assistant.ProofResult.Success(proof) =>
                replace(proof)
              case Assistant.ProofResult.SubstitutionFailure(partiallySubstitutedRule) =>
                throw RuntimeException("Substitution failure after user input")
          }

  override def quit(): Unit =
    navigation.showPopup(Navigation.Popup.Confirm("Do you want to quit the proof mode?", Some("Quit"))) { () =>
      navigation.navigateTo(Navigation.Screen.FormulaInput)
    }

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
