package proofPlayground
package frontend.tui.models

import core.logic.propositional.{Formula, FormulaF}
import core.logic.propositional.Formula.given
import core.proof.Assistant.ProofResult
import core.proof.ProofZipper.given
import core.proof.natural.Judgement
import core.proof.{Assistant, Proof, ProofSystem}
import frontend.Show.given
import frontend.tui.Navigation
import tree.Tree
import tree.Zipper.root

import java.util
import scala.compiletime.uninitialized

case class ProofStep(formula: String, rule: String)
case class ProofRule(active: Boolean, rule: String)

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

class ProofTreeModel(navigation: Navigation)(formula: Formula) extends ProofTreeModel.Data, ProofTreeModel.Signals:
  private val proofSystem    = ProofSystem.IntuitionisticPropositionalNaturalDeduction
  private val inferenceRules = proofSystem.rules.toVector.sortBy(_.label)

  // hack to remember the label of the proof step for each judgement
  // Uses `IdentityHashMap` which compares keys by reference equality (eq) instead of structural equality
  private val proofStepLabels = util.IdentityHashMap[Judgement[Formula], String]()

  private var rulesInFocus        = false
  private var zipper              = Proof(Judgement(Set.empty, formula), List.empty).zipper
  private var selected: ProofStep = uninitialized

  override def focusOnRules: Boolean    = rulesInFocus
  override def rules: Vector[ProofRule] = inferenceRules.map { rule =>
    val active = Assistant.proof(zipper.get.conclusion, rule) match
      case ProofResult.UnificationFailure()               => false
      case ProofResult.Success(proof)                     => true
      case ProofResult.SubstitutionFailure(metavariables) => true // substitution failures can be fixed by user input
    ProofRule(active, rule.label)
  }

  override def proofTree: Tree[ProofStep] =
    zipper.root.get.asTree.map { judgement =>
      val result = ProofStep(judgement.show, proofStepLabels.getOrDefault(judgement, "?"))
      // remember the current position for `nodeSelected`
      if judgement eq zipper.get.conclusion then selected = result
      result
    }

  override def isNodeSelected(node: Tree[ProofStep]): Boolean =
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
    rulesInFocus = false
    for
      idx  <- index
      rule <- inferenceRules.lift(idx)
    yield
      val replacement = Assistant.proof(zipper.get.conclusion, rule) match
        case ProofResult.UnificationFailure()               => zipper.get
        case ProofResult.Success(proof)                     => proof
        case ProofResult.SubstitutionFailure(metavariables) =>
          zipper.get // TODO handle substitution error via user input

      zipper = zipper.replace(replacement)
      proofStepLabels.put(zipper.get.conclusion, rule.label)

  override def quit(): Unit =
    navigation.showPopup("Do you want to quit the proof mode?", Some("Quit")) {
      navigation.navigateTo(Navigation.Screen.FormulaInput)
    }
