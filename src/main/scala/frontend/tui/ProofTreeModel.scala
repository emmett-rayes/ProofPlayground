package proofPlayground
package frontend.tui

enum Tree[+A]:
  case Leaf(value: A)
  case Node(value: A, children: List[Tree[A]])

object ProofModeModel:
  trait Data:
    def proofTree: Tree[String]

  trait Navigation:
    def signalExit(): Unit

class ProofModeModel extends ProofModeModel.Data:
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
