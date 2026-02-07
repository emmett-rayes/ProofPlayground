package proofPlayground
package frontend.tui

import frontend.tui.components.ConfirmPopup
import frontend.tui.views.{FormulaInput, MissingMetaVariable, ProofTree}

import tui.*
import tui.crossterm.Event

extension (frame: Frame)
  private def renderer: Renderer = new Renderer:
    override def render(widget: Widget, area: Rect): Unit =
      frame.renderWidget(widget, area)

    override def render[W <: StatefulWidget](widget: W, area: Rect)(state: widget.State): Unit =
      frame.renderStatefulWidget(widget, area)(state)

    override def setCursor(x: Int, y: Int): Unit =
      frame.setCursor(x, y)

class Coordinator extends Navigation:
  private var mainScreen: MainScreen = MainScreen(List.empty)
  private var screens: List[Screen]  = List.empty

  {
    navigateTo(Navigation.Screen.FormulaInput)
  }

  def shouldExit: Boolean = screens.isEmpty

  def handleEvent(event: Event): Unit = mainScreen.handleEvent(event)

  def render(frame: Frame): Unit =
    mainScreen = MainScreen(screens)
    mainScreen.render(frame.renderer, frame.size)

  override def exit(): Unit =
    screens = List.empty

  override def navigateTo(destination: Navigation.Screen): Unit =
    val screen = destination match
      case Navigation.Screen.FormulaInput       => FormulaInput(this)
      case Navigation.Screen.ProofTree(formula) => ProofTree(this)(formula)
    screens = screen :: List.empty

  override def showPopup(popup: Navigation.Popup)(callback: popup.Callback): Unit =
    // we can later dismiss by screen reference to allow for stacked popups with z-index offset
    val dismiss = { () => screens = screens.tail }
    val screen  = popup match
      case p @ Navigation.Popup.Confirm(message, title) =>
        ConfirmPopup(message, title)(callback.asInstanceOf[p.Callback], dismiss)
      case p @ Navigation.Popup.MissingMetaVariable(metavariable, rule) =>
        MissingMetaVariable(metavariable, rule)(callback.asInstanceOf[p.Callback], dismiss)
    screens = screen :: screens
