package proofPlayground
package frontend.tui

import tui.withTerminal

class Coordinator extends Navigation:
  private var screen: Option[Screen] = None

  navigateTo(Navigation.Screen.FormulaInput)

  def shouldExit: Boolean   = screen.isEmpty
  def currentScreen: Screen = screen.get

  override def signalExit(): Unit =
    screen = None

  override def navigateTo(screen: Navigation.Screen): Unit =
    this.screen = screen match
      case Navigation.Screen.FormulaInput       => Some(FormulaInput(this))
      case Navigation.Screen.ProofTree(formula) => Some(ProofTree(this))

@main
def main(): Unit =
  val coordinator = Coordinator()
  withTerminal { (jni, terminal) =>
    while !coordinator.shouldExit do
      terminal.draw(coordinator.currentScreen.render)
      coordinator.currentScreen.handleEvent(jni.read())
  }
