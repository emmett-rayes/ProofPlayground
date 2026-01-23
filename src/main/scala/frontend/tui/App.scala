package proofPlayground
package frontend.tui

import tui.withTerminal

class Navigation:
  private var shouldExit: Boolean = false
  private var screen: Screen      =
    FormulaInput(
      submit = formula => (),
      exit = () => shouldExit = true,
    )

  def exited: Boolean       = shouldExit
  def currentScreen: Screen = screen

@main
def main(): Unit =
  val navigation = Navigation()
  withTerminal { (jni, terminal) =>
    while !navigation.exited do
      terminal.draw(navigation.currentScreen.render)
      navigation.currentScreen.handleEvent(jni.read())
  }
