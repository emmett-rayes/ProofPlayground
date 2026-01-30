package proofPlayground
package frontend.tui.models

import frontend.tui.Navigation
import frontend.tui.Navigation.Screen
import tree.TreeZipper.given
import tree.Zipper.root
import tree.{Tree, TreeZipper}

object ProofTreeModel:
  trait Data:
    type ProofStep = (formula: String, rule: String)

    def proofTree: Tree[ProofStep]
    def selectedNode: Tree[ProofStep]

  trait Signals:
    def up(): Unit
    def down(): Unit
    def left(): Unit
    def right(): Unit
    def select(): Unit
    def quit(): Unit

class ProofTreeModel(navigation: Navigation) extends ProofTreeModel.Data, ProofTreeModel.Signals:
  private val initial = Tree(
    ("A ∧ B", "∧I"),
    List(
      Tree(("A", "AE"), List(Tree(("...", "Ax")), Tree(("...", "Ax")))),
      Tree(("B", "BE"), List(Tree(("...", "Ax")), Tree(("...", "Ax")), Tree(("...", "Ax")))),
    )
  )

  private var zipper = TreeZipper(initial)

  override def selectedNode: Tree[ProofStep] = zipper.subtree
  override def proofTree: Tree[ProofStep]    = zipper.root.get

  override def up(): Unit =
    zipper = zipper.down.getOrElse(zipper) // proof trees are upside down

  override def down(): Unit =
    zipper = zipper.up.getOrElse(zipper) // proof trees are upside down

  override def left(): Unit =
    zipper = zipper.left.getOrElse(zipper)

  override def right(): Unit =
    zipper = zipper.right.getOrElse(zipper)

  override def select(): Unit =
    val replacement = Tree(("X", "XE"), List(Tree(("...", "Ax")), Tree(("...", "Ax"))))
    zipper = zipper.replace(replacement)

  override def quit(): Unit =
    navigation.showPopup("Do you want to quit the proof mode?", Some("Quit")) {
      navigation.navigateTo(Screen.FormulaInput)
    }
