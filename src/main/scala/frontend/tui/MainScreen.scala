package proofPlayground
package frontend.tui

import tui.*
import tui.crossterm.Event
import tui.widgets.{BlockWidget, ParagraphWidget}

class MainScreen(currentScreen: Screen) extends Screen:
  override def headerText: Text                = currentScreen.headerText
  override def footerText: Text                = currentScreen.footerText
  override def handleEvent(event: Event): Unit = currentScreen.handleEvent(event)

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
    currentScreen.render(renderer, layout(2))
    renderer.render(footer, layout(4))
