package proofPlayground
package frontend.tui.models

import core.logic.propositional.Formula
import core.proof.ProofZipper.given
import core.proof.natural.Judgement
import core.proof.{Assistant, Proof, ProofSystem}
import frontend.Show.given
import frontend.tui.Navigation
import frontend.tui.Navigation.Screen
import tree.Tree
import tree.Zipper.root

import scala.compiletime.uninitialized

case class ProofStep(formula: String, rule: String)

object ProofTreeModel:
  trait Data:
    def proofTree: Tree[ProofStep]
    def nodeSelected(node: Tree[ProofStep]): Boolean

  trait Signals:
    def up(): Unit
    def down(): Unit
    def left(): Unit
    def right(): Unit
    def select(): Unit
    def quit(): Unit

class ProofTreeModel(navigation: Navigation)(formula: Formula) extends ProofTreeModel.Data, ProofTreeModel.Signals:
  private val system  = ProofSystem.IntuitionisticPropositionalNaturalDeduction
  private val initial = Proof(Judgement(Set.empty, formula), List.empty)

  private var zipper              = initial.zipper
  private var selected: ProofStep = uninitialized

  override def proofTree: Tree[ProofStep] =
    zipper.root.get.asTree.map { judgement =>
      val result = ProofStep(judgement.show, "")
      // remember the current position for `nodeSelected`
      if judgement eq zipper.get.conclusion then selected = result
      result
    }

  override def nodeSelected(node: Tree[ProofStep]): Boolean =
    selected eq node.value

  override def up(): Unit =
    zipper = zipper.down.getOrElse(zipper) // proof trees are upside down

  override def down(): Unit =
    zipper = zipper.up.getOrElse(zipper) // proof trees are upside down

  override def left(): Unit =
    zipper = zipper.left.getOrElse(zipper)

  override def right(): Unit =
    zipper = zipper.right.getOrElse(zipper)

  override def select(): Unit =
    import core.proof.natural.InferenceRules.IntuitionisticPropositional.ConjunctionIntroduction
    val replacement = Assistant.proof(zipper.get.conclusion, ConjunctionIntroduction).getOrElse(zipper.get)
    zipper = zipper.replace(replacement)

  override def quit(): Unit =
    navigation.showPopup("Do you want to quit the proof mode?", Some("Quit")) {
      navigation.navigateTo(Screen.FormulaInput)
    }
