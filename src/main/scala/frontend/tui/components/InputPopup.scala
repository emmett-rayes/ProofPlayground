package proofPlayground
package frontend.tui.components

import frontend.tui.Screen.EventResult
import frontend.tui.{Rectangle, Renderer, Screen}

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ClearWidget, ParagraphWidget}

class InputPopup(message: String, title: Option[String] = None, inputTitle: Option[String] = None)(
  confirm: String => Either[Unit, String],
  dismiss: () => Unit,
) extends Screen {
  private val ySize = 40
  private val xSize = 30

  private val textInput = TextInput(callback, inputTitle, startInEditMode = true)

  override def headerText: Text = Text.nostyle(title.getOrElse(""))

  override def footerText: Text = textInput.footerText

  override def handleEvent(event: Event): EventResult =
    event match {
      case key: Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Esc => dismiss(); EventResult.Handled
          case _              => textInput.handleEvent(event)
        }
      case _ => EventResult.NotHandled
    }

  override def render(renderer: Renderer, area: Rect): Unit = {
    val contentArea = Rectangle(ySize, xSize, area)

    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(0, 2),
      constraints = Array(
        Constraint.Percentage(30), // spacer
        Constraint.Min(1),         // message
        Constraint.Length(6),      // input
        Constraint.Length(1),      // spacer (magic)
        Constraint.Min(1),         // spacer
      )
    ).split(contentArea)

    val content    = ParagraphWidget(text = Text.nostyle(message), alignment = Alignment.Center)
    val background = ClearWidget
    val border     = BlockWidget(title = title.map(Spans.nostyle), borders = Borders.ALL)

    renderer.render(background, contentArea)
    renderer.render(border, contentArea)
    renderer.render(content, layout(1))
    textInput.render(renderer, layout(2))
  }

  private def callback(input: String) = {
    dismiss()
    confirm(input)
  }
}
