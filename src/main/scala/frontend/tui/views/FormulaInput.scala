package proofPlayground
package frontend.tui.views

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ListWidget, ParagraphWidget}

import frontend.tui.Screen.EventResult
import frontend.tui.components.TextInput
import frontend.tui.{Navigation, Renderer, Screen}
import frontend.presentation.FormulaInputModel
import frontend.presentation.FormulaInputModel.ProofSystemChoice

object FormulaInput {
  def apply(navigation: Navigation): FormulaInput = {
    val model = FormulaInputModel(navigation)
    new FormulaInput(model)(model)
  }
}

class FormulaInput(data: FormulaInputModel.Data)(signals: FormulaInputModel.Signals) extends Screen {
  private val systems         = ProofSystemChoice.values
  private val systemListState = ListWidget.State()
  private val textInput       = TextInput(inputHandler, Some(" Formula "))

  {
    systemListState.select(Some(0))
  }

  private def inputHandler: String => Either[Unit, String] =
    input => data.inputHandler(selectedSystem)(input)

  private def selectedSystem: Option[ProofSystemChoice] =
    systemListState.selected.flatMap(systems.lift)

  override def headerText: Text =
    Text.from(Span.styled("Proof Playground", Style.DEFAULT.fg(Color.Cyan).addModifier(Modifier.BOLD)))

  override def handleEvent(event: Event): EventResult =
    textInput.handleEvent(event) match {
      case EventResult.Handled    => EventResult.Handled
      case EventResult.NotHandled =>
        event match {
          case key: Event.Key =>
            key.keyEvent().code() match {
              case c: KeyCode.Char if c.c == 'q'         => signals.quit(); EventResult.Handled
              case c: KeyCode.Up if textInput.isNormal   => previousSystem(); EventResult.Handled
              case c: KeyCode.Down if textInput.isNormal => nextSystem(); EventResult.Handled
              case _                                     => EventResult.NotHandled
            }
          case _ => EventResult.NotHandled
        }
    }

  override def render(renderer: Renderer, area: Rect): Unit = {
    val systemWidth = systems.map(_.label.length).max + 4
    val layout      = Layout(
      direction = Direction.Horizontal,
      margin = Margin(1),
      constraints = Array(
        Constraint.Min(systemWidth), // system choice
        Constraint.Min(0),           // formula input
      ),
    ).split(area)

    renderSystemList(renderer, layout(0))
    textInput.render(renderer, layout(1))
  }

  private def renderSystemList(renderer: Renderer, area: Rect): Unit = {
    val active = textInput.isNormal
    val items = systems.map { system =>
      val label = Text.nostyle(system.label)
      ListWidget.Item(label, Style(bg = Some(Color.Reset), fg = Some(if active then Color.White else Color.Gray)))
    }

    val list = ListWidget(
      items = items,
      block = Some(BlockWidget(
        title = Some(Spans.nostyle(" Proof System ")),
        titleAlignment = Alignment.Center,
        borders = Borders.ALL,
        borderStyle = if active then Style(fg = Some(Color.Yellow)) else Style.DEFAULT,
      )),
      highlightStyle = if active then Style(bg = Some(Color.LightYellow)) else Style(bg = Some(Color.White)),
    )

    renderer.render(list, area)(systemListState)
  }

  override def footerText: Text = {
    val inputSpans = textInput.footerText.lines.flatMap(_.spans)
    val extraSpans =
      if !textInput.isNormal then Seq.empty
      else
        Seq(
          Span.nostyle(" Press "),
          Span.styled("Arrow Up/Down", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to select proof system, "),
          Span.styled("q", Style.DEFAULT.addModifier(Modifier.BOLD)),
          Span.nostyle(" to exit."),
        )
    Text.from(Spans.from(inputSpans ++ extraSpans*))
  }

  private def previousSystem(): Unit = {
    val i = systemListState.selected match {
      case Some(i) => if i == 0 then systems.length - 1 else i - 1
      case None    => 0
    }
    systemListState.select(Some(i))
  }

  private def nextSystem(): Unit = {
    val i = systemListState.selected match {
      case Some(i) => if i >= systems.length - 1 then 0 else i + 1
      case None    => 0
    }
    systemListState.select(Some(i))
  }
}
