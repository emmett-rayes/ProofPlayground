package proofPlayground
package frontend.tui

enum Tree[+A]:
  case Leaf(value: A)
  case Node(value: A, children: List[Tree[A]])

object ProofModeModel:
  trait Data:
    def proofTree: Tree[String]

  trait Signals:
    def quit(): Unit

class ProofModeModel(navigation: Navigation) extends ProofModeModel.Data, ProofModeModel.Signals:
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
      navigation.signalExit()
    }
