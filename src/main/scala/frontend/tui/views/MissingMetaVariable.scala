package proofPlayground
package frontend.tui.views

import core.logic.propositional.{Formula, FormulaF}
import core.meta.MetaVariable
import core.proof.InferenceRule
import core.proof.natural.Judgement
import frontend.tui.Screen.EventResult
import frontend.tui.components.TextInput
import frontend.tui.{Rectangle, Renderer, Screen}
import frontend.presentation.MissingMetaVariableModel

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ClearWidget, ParagraphWidget}

object MissingMetaVariable:
  def apply(metavariable: MetaVariable, rule: InferenceRule[Judgement, FormulaF])(
    confirm: Formula => Unit,
    dismiss: () => Unit
  ): MissingMetaVariable =
    val model = MissingMetaVariableModel(confirm, dismiss)(metavariable, rule)
    new MissingMetaVariable(model)(model)

class MissingMetaVariable(data: MissingMetaVariableModel.Data)(signals: MissingMetaVariableModel.Signals)
    extends Screen:
  private val ySize = 60
  private val xSize = 40

  private val textInput = TextInput(data.inputHandler, Some(" Formula "), startInEditMode = true)

  override def headerText: Text = Text.nostyle("")
  override def footerText: Text = textInput.footerText

  override def handleEvent(event: Event): Screen.EventResult =
    event match {
      case key: Event.Key =>
        key.keyEvent().code() match
          case c: KeyCode.Esc => signals.exit(); EventResult.Handled
          case _              => textInput.handleEvent(event)
      case _ => EventResult.NotHandled
    }

  override def render(renderer: Renderer, area: Rect): Unit =
    val contentArea = Rectangle(ySize, xSize, area)

    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(0, 4),
      constraints = Array(
        Constraint.Length(2), // spacer
        Constraint.Length(3), // rule
        Constraint.Length(2), // spacer
        Constraint.Length(1), // message
        Constraint.Min(1),    // input
        Constraint.Length(1), // spacer
      ),
    ).split(contentArea)

    val message = ParagraphWidget(
      alignment = Alignment.Center,
      text =
        Text.from(
          Span.nostyle("Enter formula for missing meta-variable "),
          Span.styled(data.variable, Style.DEFAULT.fg(Color.Red))
        ),
    )

    val border =
      BlockWidget(borders = Borders.ALL, borderType = BlockWidget.BorderType.Rounded)
    val background = ClearWidget

    renderer.render(background, contentArea)
    renderer.render(border, contentArea)
    renderRule(data.inferenceRule, renderer, layout(1))
    renderer.render(message, layout(3))
    textInput.render(renderer, layout(4))

  private def renderRule(rule: MissingMetaVariableModel.InferenceRuleString, renderer: Renderer, area: Rect): Unit =
    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(0, 4),
      constraints = Array(
        Constraint.Length(1), // premises
        Constraint.Length(1), // separator
        Constraint.Length(1), // conclusion
      )
    ).split(area)

    val premisesLayout = Layout(
      direction = Direction.Horizontal,
      constraints = Array.fill(rule.premises.length)(Constraint.Ratio(1, rule.premises.length)),
    ).split(layout(0))

    val divider = ParagraphWidget(
      text = Text.from(Span.nostyle("")),
      block =
        Some(BlockWidget(
          borders = Borders.TOP,
          title = Some(Spans.nostyle(s"[${rule.label}]")),
          titleAlignment = Alignment.Right,
        )),
    )
    val conclusion = ParagraphWidget(text = Text.nostyle(rule.conclusion), alignment = Alignment.Center)

    renderer.render(divider, layout(1))
    renderer.render(conclusion, layout(2))

    rule.premises.zipWithIndex.foreach { (h, idx) =>
      val spacer = if idx == rule.premises.length - 1 then "" else "  "
      val text   = findVariable(h) match {
        case None =>
          Text.nostyle(h + spacer)
        case Some(index) =>
          val before = h.substring(0, index)
          val after  = h.substring(index + data.variable.length)
          Text.from(
            Span.nostyle(before),
            Span.styled(data.variable, Style.DEFAULT.fg(Color.Red)),
            Span.nostyle(after + spacer)
          )
      }
      val premise = ParagraphWidget(text = text, alignment = Alignment.Center)
      renderer.render(premise, premisesLayout(idx))
    }

  private def findVariable(text: String): Option[Int] =
    text.indexOf(data.variable) match
      case -1       => None
      case varIndex =>
        val charBefore = if varIndex == 0 then ' ' else text(varIndex - 1)
        val charAfter  =
          if varIndex + data.variable.length >= text.length then ' ' else text(varIndex + data.variable.length)
        val isBoundary = (charBefore.isWhitespace || charBefore == '(') && (charAfter.isWhitespace || charAfter == ')')
        Option.when(isBoundary)(varIndex)
