package proofPlayground
package frontend.tui

import frontend.tui.models.{FormulaInputModel, InputMode}

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ParagraphWidget}

object FormulaInput:
  def apply(navigation: Navigation): FormulaInput =
    val model = FormulaInputModel(navigation)
    new FormulaInput(model)(model)

class FormulaInput(data: FormulaInputModel.Data)(signals: FormulaInputModel.Signals) extends Screen:
  override def headerText: Text =
    Text.from(Span.styled("Proof Playground", Style.DEFAULT.fg(Color.Cyan).addModifier(Modifier.BOLD)))

  override def handleEvent(event: Event): Unit =
    event match {
      case key: tui.crossterm.Event.Key =>
        data.mode match {
          case InputMode.Normal =>
            key.keyEvent().code() match
              case c: KeyCode.Enter              => signals.edit()
              case c: KeyCode.Char if c.c == 'q' => signals.quit()
              case c: KeyCode.Char if c.c == 'c' => signals.clear()
              case _                             => ()
          case InputMode.Editing =>
            key.keyEvent().code() match {
              case c: KeyCode.Enter     => signals.submit()
              case c: KeyCode.Esc       => signals.back()
              case c: KeyCode.Backspace => signals.backspace()
              case c: KeyCode.Left      => signals.cursorLeft()
              case c: KeyCode.Right     => signals.cursorRight()
              case c: KeyCode.Char      => signals.character(c.c)
              case _                    => ()
            }
          case InputMode.Error(_) =>
            key.keyEvent().code() match {
              case c: KeyCode.Enter => signals.edit()
              case c: KeyCode.Esc   => signals.edit()
              case _                => ()
            }
        }
      case _ => ()
    }

  override def render(renderer: Renderer, area: Rect): Unit =
    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(1),
      constraints = Array(
        Constraint.Length(2), // prompt
        Constraint.Length(3), // input
        Constraint.Min(0),    // spacer
      ),
    ).split(area)

    val prompt = ParagraphWidget(text = Text.from(Span.nostyle("Enter initial formula:")))

    val inputStyle = data.mode match
      case InputMode.Normal   => Style.DEFAULT
      case InputMode.Editing  => Style.DEFAULT.fg(Color.Yellow)
      case InputMode.Error(_) => Style.DEFAULT.fg(Color.Red)

    val input = ParagraphWidget(
      text = Text.from(Span.nostyle(data.formula)),
      style = inputStyle,
      block = Some(BlockWidget(borders = Borders.ALL, title = Some(Spans.nostyle(" Formula ")))),
    )

    renderer.render(prompt, layout(0))
    renderer.render(input, layout(1))
    data.mode match
      case InputMode.Editing | InputMode.Error(_) =>
        val cursorOffset = Grapheme(data.formula.take(data.cursor)).width
        renderer.setCursor(x = layout(1).x + cursorOffset + 1, y = layout(1).y + 1)
      case _ => ()

  override def footerText: Text =
    data.mode match
      case InputMode.Normal =>
        Text.from(
          Span.nostyle("Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to edit, "),
          Span.styled("c", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to clear input, "),
          Span.styled("q", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to exit."),
        )
      case InputMode.Editing =>
        Text.from(
          Span.nostyle("Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to submit, "),
          Span.styled("Esc", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to cancel."),
        )
      case InputMode.Error(message) =>
        Text.from(
          Span.styled("Error: ", Style.DEFAULT.fg(Color.Red).addModifier(Modifier.BOLD)),
          Span.styled(message, Style.DEFAULT.fg(Color.Red)),
          Span.nostyle(" Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" or "),
          Span.styled("Esc", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to continue editing."),
        )
