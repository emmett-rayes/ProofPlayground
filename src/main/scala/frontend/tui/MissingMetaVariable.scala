package proofPlayground
package frontend.tui

import core.logic.propositional.{Formula, FormulaF}
import core.meta.MetaVariable
import core.proof.InferenceRule
import core.proof.natural.Judgement
import frontend.tui.components.InputPopup
import frontend.tui.models.MissingMetaVariableModel

import tui.crossterm.Event
import tui.{Rect, Text}

object MissingMetaVariable:
  def apply(metavariable: MetaVariable, rule: InferenceRule[Judgement, FormulaF])(
    confirm: Formula => Unit,
    dismiss: () => Unit
  ): MissingMetaVariable =
    val model = MissingMetaVariableModel(confirm, dismiss)(metavariable, rule)
    new MissingMetaVariable(model)

class MissingMetaVariable(data: MissingMetaVariableModel.Data) extends Screen:
  private val popup = InputPopup(
    s"Enter formula for missing meta-variable ${data.variable}",
    Some("Missing meta-variable"),
    Some(" Formula "),
  )(data.inputHandler, data.exitHandler)

  override def headerText: Text = popup.headerText
  override def footerText: Text = popup.footerText

  override def handleEvent(event: Event): Screen.EventResult = popup.handleEvent(event)

  override def render(renderer: Renderer, area: Rect): Unit =
    popup.render(renderer, area)
