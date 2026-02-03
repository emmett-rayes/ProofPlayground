package proofPlayground
package frontend.tui

import core.logic.propositional.{Formula, FormulaF}
import core.meta.MetaVariable
import core.proof.InferenceRule
import core.proof.natural.Judgement
import frontend.tui.components.InputPopup
import frontend.tui.models.MissingMetaVariableModel

import tui.*
import tui.crossterm.Event
import tui.widgets.{BlockWidget, ClearWidget, ParagraphWidget}

object MissingMetaVariable:
  def apply(metavariable: MetaVariable, rule: InferenceRule[Judgement, FormulaF])(
    confirm: Formula => Unit,
    dismiss: () => Unit
  ): MissingMetaVariable =
    val model = MissingMetaVariableModel(confirm, dismiss)(metavariable, rule)
    new MissingMetaVariable(model)

class MissingMetaVariable(data: MissingMetaVariableModel.Data) extends Screen:
  private val ySize = 60
  private val xSize = 40

  private val popup = InputPopup(
    s"Enter formula for missing meta-variable ${data.variable}",
    Some("Missing meta-variable"),
    Some(" Formula "),
  )(data.inputHandler, data.exitHandler, ySize = 100, xSize = 100) // Hack for defining actual popup size in this class

  override def headerText: Text = popup.headerText
  override def footerText: Text = popup.footerText

  override def handleEvent(event: Event): Screen.EventResult = popup.handleEvent(event)

  override def render(renderer: Renderer, area: Rect): Unit =
    val contentArea = Rectangle(ySize, xSize, area)

    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(0, 4),
      constraints = Array(
        Constraint.Length(2), // spacer
        Constraint.Length(3), // rule
        Constraint.Length(2), // spacer
        Constraint.Min(1),    // input
      ),
    ).split(contentArea)

    val border =
      BlockWidget(borders = Borders.ALL, borderType = BlockWidget.BorderType.Rounded)
    val background = ClearWidget

    renderer.render(background, contentArea)
    renderer.render(border, contentArea)
    renderRule(data.inferenceRule, renderer, layout(1))
    popup.render(renderer, layout(3))

  private def renderRule(rule: MissingMetaVariableModel.InferenceRuleString, renderer: Renderer, area: Rect): Unit =
    val layout = Layout(
      direction = Direction.Vertical,
      margin = Margin(0, 4),
      constraints = Array(
        Constraint.Length(1), // hypotheses
        Constraint.Length(1), // separator
        Constraint.Length(1), // conclusion
      )
    ).split(area)

    val hypothesesLayout = Layout(
      direction = Direction.Horizontal,
      constraints = Array.fill(rule.hypotheses.length)(Constraint.Ratio(1, rule.hypotheses.length)),
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

    rule.hypotheses.zipWithIndex.foreach { (h, idx) =>
      val spacer     = if idx == rule.hypotheses.length - 1 then "" else "  "
      val style      = if h.contains(data.variable) then Style.DEFAULT.fg(Color.Red) else Style.DEFAULT
      val hypothesis = ParagraphWidget(text = Text.nostyle(h + spacer), alignment = Alignment.Center, style = style)
      renderer.render(hypothesis, hypothesesLayout(idx))
    }
