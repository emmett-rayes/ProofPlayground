package proofPlayground
package frontend.tui.components

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ParagraphWidget}

import frontend.tui.Screen.EventResult
import frontend.tui.{Renderer, Screen}

class TextInput(
  callback: String => Either[Unit, String],
  title: Option[String] = None,
  startInEditMode: Boolean = false,
) extends Screen {
  private var cursor: Int     = 0
  private var text: String    = ""
  private var mode: InputMode = InputMode.Normal

  {
    if startInEditMode then edit()
  }

  override def handleEvent(event: Event): EventResult = {
    import scala.language.implicitConversions
    given Conversion[Unit, EventResult.Handled.type] = _ => EventResult.Handled

    event match {
      case key: Event.Key =>
        mode match {
          case InputMode.Normal =>
            key.keyEvent().code() match {
              case c: KeyCode.Enter              => edit()
              case c: KeyCode.Char if c.c == 'c' => clear()
              case _                             => EventResult.NotHandled
            }
          case InputMode.Editing =>
            key.keyEvent().code() match {
              case c: KeyCode.Enter     => submit()
              case c: KeyCode.Esc       => normal()
              case c: KeyCode.Backspace => backspace()
              case c: KeyCode.Left      => cursorLeft()
              case c: KeyCode.Right     => cursorRight()
              case c: KeyCode.Char      => character(c.c)
              case _                    => EventResult.NotHandled
            }
          case InputMode.Error(_) =>
            key.keyEvent().code() match {
              case c: KeyCode.Enter => edit()
              case c: KeyCode.Esc   => edit()
              case _                => EventResult.NotHandled
            }
        }
      case _ => EventResult.NotHandled
    }
  }

  private def backspace(): Unit =
    if mode == InputMode.Editing && cursor > 0 then {
      text = text.patch(cursor - 1, "", 1)
      cursor -= 1
    }

  private def edit(): Unit = {
    mode = InputMode.Editing
    cursor = text.length
  }

  private def normal(): Unit =
    mode match {
      case InputMode.Editing => mode = InputMode.Normal
      case _                 => ()
    }

  private def clear(): Unit =
    if mode == InputMode.Normal then {
      text = ""
      cursor = 0
    }

  private def cursorLeft(): Unit =
    if mode == InputMode.Editing && cursor > 0 then
      cursor -= 1

  private def cursorRight(): Unit =
    if mode == InputMode.Editing && cursor < text.length then
      cursor += 1

  private def character(c: Char): Unit =
    if mode == InputMode.Editing then {
      text = text.patch(cursor, c.toString, 0)
      cursor += 1
    }

  private def submit(): Unit =
    callback(text) match {
      case Left(_)    => mode = InputMode.Normal
      case Right(msg) => mode = InputMode.Error(msg)
    }

  override def render(renderer: Renderer, area: Rect): Unit = {
    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(1),
      constraints = Array(
        Constraint.Length(3), // input
        Constraint.Length(1), // error
      ),
    ).split(area)

    val inputStyle = mode match {
      case InputMode.Normal   => Style.DEFAULT
      case InputMode.Editing  => Style.DEFAULT.fg(Color.Yellow)
      case InputMode.Error(_) => Style.DEFAULT.fg(Color.Red)
    }

    val input = ParagraphWidget(
      text = Text.from(Span.nostyle(text)),
      style = inputStyle,
      block = Some(BlockWidget(borders = Borders.ALL, title = title.map(Spans.nostyle))),
    )

    val errorText = mode match {
      case InputMode.Error(message) =>
        Text.from(
          Span.styled("Error: ", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(message),
          Span.nostyle("."),
        )
      case _ => Text.nostyle("")
    }
    val error = ParagraphWidget(text = errorText, style = Style.DEFAULT.fg(Color.Red))

    renderer.render(input, layout(0))
    renderer.render(error, layout(1))

    mode match {
      case InputMode.Editing | InputMode.Error(_) =>
        val cursorOffset = Grapheme(text.take(cursor)).width
        renderer.setCursor(x = layout(0).x + cursorOffset + 1, y = layout(0).y + 1)
      case _ => ()
    }
  }

  override def headerText: Text =
    Text.nostyle("")

  override def footerText: Text =
    mode match {
      case InputMode.Normal =>
        Text.from(
          Span.nostyle("Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to edit, "),
          Span.styled("c", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to clear input."),
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
          Span.nostyle(" Press "),
          Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" or "),
          Span.styled("Esc", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to continue editing."),
        )
    }

  private enum InputMode {
    case Normal
    case Editing
    case Error(message: String)
  }
}
