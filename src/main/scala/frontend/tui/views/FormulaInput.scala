package proofPlayground
package frontend.tui.views

import frontend.tui.Screen.EventResult
import frontend.tui.components.TextInput
import frontend.tui.{Navigation, Renderer, Screen}
import frontend.presentation.FormulaInputModel

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.ParagraphWidget

object FormulaInput {
  def apply(navigation: Navigation): FormulaInput = {
    val model = FormulaInputModel(navigation)
    new FormulaInput(model)(model)
  }
}

class FormulaInput(data: FormulaInputModel.Data)(signals: FormulaInputModel.Signals) extends Screen {
  private val textInput = TextInput(data.inputHandler, Some(" Formula "))

  override def headerText: Text =
    Text.from(Span.styled("Proof Playground", Style.DEFAULT.fg(Color.Cyan).addModifier(Modifier.BOLD)))

  override def handleEvent(event: Event): EventResult =
    textInput.handleEvent(event) match {
      case EventResult.Handled    => EventResult.Handled
      case EventResult.NotHandled =>
        event match {
          case key: Event.Key =>
            key.keyEvent().code() match {
              case c: KeyCode.Char if c.c == 'q' => signals.quit(); EventResult.Handled
              case _                             => EventResult.NotHandled
            }
          case _ => EventResult.NotHandled
        }
    }

  override def render(renderer: Renderer, area: Rect): Unit = {
    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(1),
      constraints = Array(
        Constraint.Length(2), // prompt
        Constraint.Min(0),    // input
      ),
    ).split(area)

    val prompt = ParagraphWidget(text = Text.from(Span.nostyle("Enter initial formula:")))

    renderer.render(prompt, layout(0))
    textInput.render(renderer, layout(1))
  }

  override def footerText: Text = {
    textInput.footerText.lines.flatMap(_.spans)
    val spans = Spans.from(
      Span.nostyle(" Press "),
      Span.styled("q", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(" to exit."),
    )
    Text.from(Spans.from(textInput.footerText.lines.flatMap(_.spans) ++ spans.spans*))
  }
}
