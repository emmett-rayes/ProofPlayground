package proofPlayground
package frontend.tui

import tui.Frame
import tui.crossterm.Event

import tui.*
import tui.widgets.{BlockWidget, ParagraphWidget}

class Coordinator extends Navigation:
  private var screens: List[Screen] = List.empty

  {
    navigateTo(Navigation.Screen.FormulaInput)
  }

  def shouldExit: Boolean = screens.isEmpty

  def handleEvent(event: Event): Unit =
    screens.head.handleEvent(event)

  def render(frame: Frame): Unit =
    val renderer = new Renderer {
      override def render(widget: Widget, area: Rect): Unit =
        frame.renderWidget(widget, area)

      override def render[W <: StatefulWidget](widget: W, area: Rect)(state: widget.State): Unit =
        frame.renderStatefulWidget(widget, area)(state)

      override def setCursor(x: Int, y: Int): Unit =
        frame.setCursor(x, y)
    }

    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(1),
      constraints = Array(
        Constraint.Length(2), // header
        Constraint.Length(1), // spacer
        Constraint.Min(0),    // content
        Constraint.Length(1), // spacer
        Constraint.Length(2), // footer
      ),
    ).split(frame.size)

    val currentScreen = screens.head

    val header = ParagraphWidget(
      text = currentScreen.headerText,
      block = Some(BlockWidget(borders = Borders.BOTTOM, borderType = BlockWidget.BorderType.Double)),
    )

    val footer = ParagraphWidget(
      text = currentScreen.footerText,
      block = Some(BlockWidget(borders = Borders.TOP, borderType = BlockWidget.BorderType.Double)),
    )

    frame.renderWidget(header, layout(0))
    currentScreen.render(renderer, layout(2))
    frame.renderWidget(footer, layout(4))

  override def exit(): Unit =
    screens = List.empty

  override def navigateTo(destination: Navigation.Screen): Unit =
    val screen = destination match
      case Navigation.Screen.FormulaInput       => FormulaInput(this)
      case Navigation.Screen.ProofTree(formula) => ProofTree(this)
    screens = screen :: List.empty

  override def showPopup(message: String, title: Option[String])(callback: () => Unit): Unit =
    screens = Popup(message, title)(callback, { () => screens = screens.tail }) :: screens
