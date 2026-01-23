package proofPlayground
package frontend.tui

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ParagraphWidget}

class FormulaInput(submit: String => Unit, exit: () => Unit) extends Screen:
  private var mode: InputMode = InputMode.Normal
  private var formula: String = ""

  override def handleEvent(event: Event): Unit =
    event match {
      case key: tui.crossterm.Event.Key =>
        mode match {
          case InputMode.Normal =>
            key.keyEvent().code() match
              case c: KeyCode.Enter              => mode = InputMode.Editing
              case c: KeyCode.Char if c.c == 'q' => exit()
              case _                             => ()
          case InputMode.Editing =>
            key.keyEvent().code() match {
              case c: KeyCode.Enter     => submit(formula)
              case c: KeyCode.Esc       => formula = ""; mode = InputMode.Normal
              case c: KeyCode.Backspace => formula = formula.dropRight(1)
              case c: KeyCode.Char      => formula += c.c
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

    val input = ParagraphWidget(
      text = Text.from(Span.nostyle(formula)),
      style = if mode == InputMode.Editing then Style.DEFAULT.fg(Color.Yellow) else Style.DEFAULT,
      block = Some(BlockWidget(borders = Borders.ALL, title = Some(Spans.nostyle(" Formula ")))),
    )

    val footerText = mode match
      case InputMode.Normal =>
        Text.from(
          Span.nostyle("Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to edit, "),
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

    val footer = ParagraphWidget(
      text = footerText,
      block = Some(BlockWidget(borders = Borders.TOP)),
    )

    frame.renderWidget(header, chunks(0))
    frame.renderWidget(prompt, chunks(1))
    frame.renderWidget(input, chunks(2))
    frame.renderWidget(footer, chunks.last)
    if mode == InputMode.Editing then
      frame.setCursor(x = chunks(2).x + Grapheme(formula).width + 1, y = chunks(2).y + 1)

  private enum InputMode:
    case Normal
    case Editing
