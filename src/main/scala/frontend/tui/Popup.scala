package proofPlayground
package frontend.tui

import frontend.tui.Screen.EventResult

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ClearWidget, ParagraphWidget}

class Popup(message: String, title: Option[String] = None)(confirm: () => Unit, dismiss: () => Unit)
    extends Screen:
  private val ySize      = 40
  private val xSize      = 30
  private var confirming = false

  override def headerText: Text =
    Text.nostyle(title.getOrElse(""))

  override def footerText: Text =
    Text.from(
      Span.nostyle("Press "),
      Span.styled("Esc", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(" to cancel."),
    )

  override def handleEvent(event: Event): EventResult =
    import scala.language.implicitConversions
    given Conversion[Unit, EventResult.Handled.type] = _ => EventResult.Handled

    event match {
      case key: tui.crossterm.Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Esc   => dismiss()
          case c: KeyCode.Enter => dismiss(); if confirming then confirm()
          case c: KeyCode.Left  => confirming = false
          case c: KeyCode.Right => confirming = true
          case _                => EventResult.NotHandled
        }
      case _ => EventResult.NotHandled
    }

  override def render(renderer: Renderer, area: Rect): Unit =
    val contentArea = Rectangle(ySize, xSize, area)

    val contentLayout = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Percentage(35), // spacer
        Constraint.Min(1),         // message
        Constraint.Length(3),      // buttons
        Constraint.Length(1),      // spacer (magic)
        Constraint.Min(1),         // spacer
      )
    ).split(contentArea)

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

    val cancelButton  = ButtonWidget("Cancel", !confirming)
    val confirmButton = ButtonWidget("Confirm", confirming)
    val content       = ParagraphWidget(text = Text.nostyle(message), alignment = Alignment.Center)
    val background    = ClearWidget
    val border        = BlockWidget(title = title.map(Spans.nostyle), borders = Borders.ALL)

    renderer.render(background, contentArea)
    renderer.render(border, contentArea)
    renderer.render(content, contentLayout(1))
    renderer.render(cancelButton, buttonsLayout(0))
    renderer.render(confirmButton, buttonsLayout(2))

  private def Rectangle(ySize: Int, xSize: Int, area: Rect): Rect =
    val yLayout = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Percentage((100 - ySize) / 2),
        Constraint.Percentage(ySize),
        Constraint.Percentage((100 - ySize) / 2)
      )
    ).split(area)

    val xLayout = Layout(
      direction = Direction.Horizontal,
      constraints = Array(
        Constraint.Percentage((100 - xSize) / 2),
        Constraint.Percentage(xSize),
        Constraint.Percentage((100 - xSize) / 2),
      )
    ).split(yLayout(1))

    xLayout(1)

  private def ButtonWidget(label: String, active: => Boolean) =
    ParagraphWidget(
      text = Text.nostyle(label),
      alignment = Alignment.Center,
      block = Some(
        BlockWidget(
          borders = Borders.ALL,
          style = if active then Style.DEFAULT.fg(Color.Yellow) else Style.DEFAULT
        )
      )
    )
