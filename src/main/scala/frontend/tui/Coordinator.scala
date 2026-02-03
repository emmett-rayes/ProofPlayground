package proofPlayground
package frontend.tui

import frontend.tui.components.{ConfirmPopup, InputPopup}
import frontend.tui.views.{FormulaInput, ProofTree}

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
    val screen   = MainScreen(screens)
    screen.render(renderer, frame.size)

  override def exit(): Unit =
    screens = List.empty

  override def navigateTo(destination: Navigation.Screen): Unit =
    val screen = destination match
      case Navigation.Screen.FormulaInput       => FormulaInput(this)
      case Navigation.Screen.ProofTree(formula) => ProofTree(this)(formula)
    screens = screen :: List.empty

  override def showPopup(popup: Navigation.Popup)(callback: popup.Callback): Unit =
    val dismiss = { () => screens = screens.tail }
    val screen  = popup match
      case p @ Navigation.Popup.Confirm(message, title) =>
        ConfirmPopup(message, title)(callback.asInstanceOf[p.Callback], dismiss)
      case p @ Navigation.Popup.MissingMetaVariable(metavariable, rule) =>
        MissingMetaVariable(metavariable, rule)(callback.asInstanceOf[p.Callback], dismiss)
    screens = screen :: screens
