package proofPlayground
package frontend.tui

import frontend.tui.Navigation.Screen
import tree.{Tree, TreeZipper}
import tree.TreeZipper.given
import tree.Zipper.root

object ProofTreeModel:
  trait Data:
    def proofTree: Tree[String]
    def selectedNode: Tree[String]

  trait Signals:
    def up(): Unit
    def down(): Unit
    def left(): Unit
    def right(): Unit
    def select(): Unit
    def quit(): Unit

class ProofTreeModel(navigation: Navigation) extends ProofTreeModel.Data, ProofTreeModel.Signals:
  private val initial = Tree(
    "A ∧ B",
    List(
      Tree("A", List(Tree("..."), Tree("..."))),
      Tree("B", List(Tree("..."), Tree("..."))),
    )
  )

  private var zipper = TreeZipper(initial)

  override def selectedNode: Tree[String] = zipper.subtree
  override def proofTree: Tree[String]    = zipper.root.get

  override def up(): Unit =
    zipper = zipper.down.getOrElse(zipper) // proof trees are upside down

  override def down(): Unit =
    zipper = zipper.up.getOrElse(zipper) // proof trees are upside down

  override def left(): Unit =
    zipper = zipper.left.getOrElse(zipper)

  override def right(): Unit =
    zipper = zipper.right.getOrElse(zipper)

  override def select(): Unit =
    val replacement = Tree("X", List(Tree("..."), Tree("...")))
    zipper = zipper.replace(replacement)

  override def quit(): Unit =
    navigation.showPopup("Do you want to quit the proof mode?", Some("Quit")) { () =>
      navigation.navigateTo(Screen.FormulaInput)
    }
