package proofPlayground
package frontend.tui

import tui.Frame
import tui.crossterm.Event

class Coordinator extends Navigation:
  private var screens: List[Screen] = List.empty

  {
    navigateTo(Navigation.Screen.FormulaInput)
  }

  def shouldExit: Boolean = screens.isEmpty

  def handleEvent(event: Event): Unit =
    screens.head.handleEvent(event)

  def render(frame: Frame): Unit =
    val renderer = FrameRenderer(frame)
    val screen   = MainScreen(screens.head)
    screen.render(renderer, frame.size)

  override def exit(): Unit =
    screens = List.empty

  override def navigateTo(destination: Navigation.Screen): Unit =
    val screen = destination match
      case Navigation.Screen.FormulaInput       => FormulaInput(this)
      case Navigation.Screen.ProofTree(formula) => ProofTree(this)
    screens = screen :: List.empty

  override def showPopup(message: String, title: Option[String])(callback: => Unit): Unit =
    screens = Popup(message, title)({ () => callback }, { () => screens = screens.tail }) :: screens
