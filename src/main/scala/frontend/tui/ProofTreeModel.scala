package proofPlayground
package frontend.tui

import frontend.tui.Navigation.Screen
import tree.Tree

object ProofTreeModel:
  trait Data:
    def proofTree: Tree[String]
    def selectedNode: Tree[String]

  trait Signals:
    def quit(): Unit

class ProofTreeModel(navigation: Navigation) extends ProofTreeModel.Data, ProofTreeModel.Signals:
  private var currentNode = proofTree

  override def selectedNode: Tree[String] = currentNode

  override def proofTree: Tree[String] =
    Tree.Node(
      "A ∧ B",
      List(
        Tree.Node(
          "A",
          List(
            Tree.Leaf("..."),
            Tree.Leaf("..."),
          ),
        ),
        Tree.Node(
          "B",
          List(
            Tree.Leaf("..."),
            Tree.Leaf("..."),
          ),
        ),
      ),
    )

  override def quit(): Unit =
    navigation.showPopup("Do you want to quit the proof mode?", Some("Quit")) { () =>
      navigation.navigateTo(Screen.FormulaInput)
    }
