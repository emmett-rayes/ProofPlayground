package proofPlayground
package frontend.tui

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ParagraphWidget}

class Popup(message: String, title: Option[String])(confirm: () => Unit, dismiss: () => Unit) extends Screen:
  private val size       = 30
  private var confirming = false

  override def handleEvent(event: Event): Unit =
    event match {
      case key: tui.crossterm.Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Esc   => dismiss()
          case c: KeyCode.Enter => if confirming then confirm() else dismiss()
          case c: KeyCode.Left  => confirming = false
          case c: KeyCode.Right => confirming = true
          case _                => ()
        }
      case _ => ()
    }

  override def render(frame: Frame): Unit =
    val yLayout = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Percentage((100 - size) / 2),
        Constraint.Percentage(size),
        Constraint.Percentage((100 - size) / 2)
      )
    ).split(frame.size)

    val xLayout = Layout(
      direction = Direction.Horizontal,
      constraints = Array(
        Constraint.Percentage((100 - size) / 2),
        Constraint.Percentage(size),
        Constraint.Percentage((100 - size) / 2),
      )
    ).split(yLayout(1))

    val contentLayout = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Percentage(35), // spacer
        Constraint.Min(1),         // message
        Constraint.Length(3),      // buttons
        Constraint.Length(1),      // spacer (magic)
        Constraint.Min(1),         // spacer
      )
    ).split(xLayout(1))

    val buttonsBarLayout = Layout(
      direction = Direction.Horizontal,
      constraints = Array(
        Constraint.Percentage(25), // spacer
        Constraint.Percentage(50), // buttons
        Constraint.Percentage(25), // spacer
      )
    ).split(contentLayout(2))

    val buttonsLayout = Layout(
      direction = Direction.Horizontal,
      constraints = Array(
        Constraint.Max(13), // cancel
        Constraint.Min(4),  // spacer
        Constraint.Max(13), // confirming
      )
    ).split(buttonsBarLayout(1))

    val cancelButton  = ButtonWidget(" Cancel", () => !confirming)
    val confirmButton = ButtonWidget("Confirm", () => confirming)
    val content       = ParagraphWidget(text = Text.nostyle(message), alignment = Alignment.Center)
    val border        = BlockWidget(title = title.map(Spans.nostyle), borders = Borders.ALL)

    frame.renderWidget(border, xLayout(1))
    frame.renderWidget(content, contentLayout(1))
    frame.renderWidget(cancelButton, buttonsLayout(0))
    frame.renderWidget(confirmButton, buttonsLayout(2))

  private def ButtonWidget(label: String, active: () => Boolean) =
    ParagraphWidget(
      text = Text.nostyle(label),
      alignment = Alignment.Center,
      block = Some(
        BlockWidget(
          borders = Borders.ALL,
          style = if active() then Style.DEFAULT.fg(Color.Yellow) else Style.DEFAULT
        )
      )
    )
