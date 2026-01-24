package proofPlayground
package frontend.tui

import core.logic.propositional.Formula

import tui.withTerminal

class Coordinator:
  private var shouldExit: Boolean = false
  private var screen: Screen      = FormulaInput(
    new FormulaInputModel.Navigation:
      override def signalExit(): Unit =
        shouldExit = true
      override def submitFormula(formula: Formula): Unit =
        screen = ProofTree()
  )

  def exited: Boolean       = shouldExit
  def currentScreen: Screen = screen

@main
def main(): Unit =
  val coordinator = Coordinator()
  withTerminal { (jni, terminal) =>
    while !coordinator.exited do
      terminal.draw(coordinator.currentScreen.render)
      coordinator.currentScreen.handleEvent(jni.read())
  }
