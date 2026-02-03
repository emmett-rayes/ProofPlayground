package proofPlayground
package frontend.tui

import frontend.tui.Screen.EventResult

import tui.*
import tui.crossterm.Event
import tui.widgets.{BlockWidget, ParagraphWidget}

class MainScreen(screens: List[Screen]) extends Screen:
  override def handleEvent(event: Event): EventResult = screens.head.handleEvent(event)

  override def render(renderer: Renderer, area: Rect): Unit =
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
    ).split(area)

    val header = ParagraphWidget(
      text = headerText,
      block = Some(BlockWidget(borders = Borders.BOTTOM, borderType = BlockWidget.BorderType.Double)),
    )

    val footer = ParagraphWidget(
      text = footerText,
      block = Some(BlockWidget(borders = Borders.TOP, borderType = BlockWidget.BorderType.Double)),
    )

    renderer.render(header, layout(0))
    screens.reverse.foreach { screen => screen.render(renderer, layout(2)) }
    renderer.render(footer, layout(4))

  override def headerText: Text = screens.head.headerText
  override def footerText: Text = screens.head.footerText
