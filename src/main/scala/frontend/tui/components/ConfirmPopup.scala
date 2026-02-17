package proofPlayground
package frontend.tui.components

import frontend.tui.Screen.EventResult
import frontend.tui.{Rectangle, Renderer, Screen}

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ClearWidget, ParagraphWidget}

class ConfirmPopup(message: String, title: Option[String] = None)(confirm: Option[() => Unit], dismiss: () => Unit)
    extends Screen:
  private val ySize = 40
  private val xSize = 30

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
      case key: Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Esc   => dismiss()
          case c: KeyCode.Enter => dismiss(); if confirming then confirm.foreach(_.apply())
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
      margin = Margin(0, 1),
      constraints = Array(
        Constraint.Percentage(22), // spacer
        Constraint.Percentage(56), // buttons
        Constraint.Percentage(22), // spacer
      )
    ).split(contentLayout(2))

    val buttonsLayout =
      if confirm.isDefined then
        Layout(
          direction = Direction.Horizontal,
          constraints = Array(
            Constraint.Max(13), // cancel
            Constraint.Min(3),  // spacer
            Constraint.Max(13), // confirm
          )
        ).split(buttonsBarLayout(1))
      else
        Layout(
          direction = Direction.Horizontal,
          constraints = Array(
            Constraint.Max(5), // ok
          )
        ).split(buttonsBarLayout(1))

    val cancelButton  = ButtonWidget(if confirm.isDefined then " Cancel" else "Ok", !confirming)
    val confirmButton = ButtonWidget("Confirm", confirming)
    val content       = ParagraphWidget(
      text = Text.nostyle(message),
      alignment = Alignment.Center,
      wrap = Some(ParagraphWidget.Wrap(true)),
    )
    val background = ClearWidget
    val border     = BlockWidget(title = title.map(Spans.nostyle), borders = Borders.ALL)

    renderer.render(background, contentArea)
    renderer.render(border, contentArea)
    renderer.render(content, contentLayout(1))
    renderer.render(cancelButton, buttonsLayout(0))
    if confirm.isDefined then
      renderer.render(confirmButton, buttonsLayout(2))

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
