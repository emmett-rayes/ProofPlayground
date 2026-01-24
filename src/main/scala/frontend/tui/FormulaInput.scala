package proofPlayground
package frontend.tui

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ParagraphWidget}

object FormulaInput:
  def apply(shouldExit: () => Unit): FormulaInput =
    val model = FormulaInputModel(shouldExit)
    new FormulaInput(model)(model)

class FormulaInput(data: FormulaInputModel.Data)(signals: FormulaInputModel.Signals) extends Screen:
  override def handleEvent(event: Event): Unit =
    event match {
      case key: tui.crossterm.Event.Key =>
        data.mode match {
          case InputMode.Normal =>
            key.keyEvent().code() match
              case c: KeyCode.Enter              => signals.edit()
              case c: KeyCode.Char if c.c == 'q' => signals.quit()
              case _                             => ()
          case InputMode.Editing | InputMode.Error(_) =>
            key.keyEvent().code() match {
              case c: KeyCode.Enter     => signals.submit()
              case c: KeyCode.Esc       => signals.clear()
              case c: KeyCode.Backspace => signals.backspace()
              case c: KeyCode.Left      => signals.cursorLeft()
              case c: KeyCode.Right     => signals.cursorRight()
              case c: KeyCode.Char      => signals.character(c.c)
              case _                    => ()
            }
        }
      case _ => ()
    }

  override def render(frame: Frame): Unit =
    val chunks = Layout(
      direction = Direction.Vertical,
      margin = Margin(1),
      constraints = Array(
        Constraint.Length(2), // header
        Constraint.Length(2), // prompt
        Constraint.Length(3), // input
        Constraint.Min(0),    // spacer
        Constraint.Length(2), // input
      ),
    ).split(frame.size)

    val header = ParagraphWidget(
      text = Text.from(Span.styled("Proof Playground", Style.DEFAULT.fg(Color.Cyan).addModifier(Modifier.BOLD))),
      block = Some(BlockWidget(borders = Borders.BOTTOM)),
    )

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

    val footerText = data.mode match
      case InputMode.Normal =>
        Text.from(
          Span.nostyle("Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to edit, "),
          Span.styled("q", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to exit."),
        )
      case InputMode.Editing | InputMode.Error(_) =>
        Text.from(
          Span.nostyle("Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to submit, "),
          Span.styled("Esc", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to cancel."),
        )

    val footer = ParagraphWidget(
      text = footerText,
      block = Some(BlockWidget(borders = Borders.TOP)),
    )

    frame.renderWidget(header, chunks(0))
    frame.renderWidget(prompt, chunks(1))
    frame.renderWidget(input, chunks(2))
    frame.renderWidget(footer, chunks.last)
    data.mode match
      case InputMode.Editing | InputMode.Error(_) =>
        // Place the cursor at the correct position in the input box
        val cursorOffset = Grapheme(data.formula.take(data.cursor)).width
        frame.setCursor(x = chunks(2).x + cursorOffset + 1, y = chunks(2).y + 1)
      case _ => ()
