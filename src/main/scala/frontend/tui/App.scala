package proofPlayground
package frontend.tui

import tui.withTerminal

class Coordinator extends Navigation:
  private var screens: List[Screen] = List.empty

  {
    navigateTo(Navigation.Screen.FormulaInput)
  }

  def shouldExit: Boolean   = screens.isEmpty
  def currentScreen: Screen = screens.head

  override def signalExit(): Unit =
    screens = List.empty

  override def navigateTo(destination: Navigation.Screen): Unit =
    val screen = destination match
      case Navigation.Screen.FormulaInput       => FormulaInput(this)
      case Navigation.Screen.ProofTree(formula) => ProofTree(this)
    screens = screen :: tail

  override def showPopup(message: String, title: Option[String])(callback: () => Unit): Unit =
    screens = Popup(message, title)(callback, { () => screens = tail }) :: screens

  private def tail: List[Screen] =
    if screens.isEmpty then List.empty else screens.tail

@main
def main(): Unit =
  val coordinator = Coordinator()
  withTerminal { (jni, terminal) =>
    while !coordinator.shouldExit do
      terminal.draw(coordinator.currentScreen.render)
      coordinator.currentScreen.handleEvent(jni.read())
  }
