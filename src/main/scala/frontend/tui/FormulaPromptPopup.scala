/*
package proofPlayground
package frontend.tui

import core.logic.propositional.Formula
import frontend.tui.models.FormulaInputModel

import proofPlayground.frontend.tui.components.TextInput
import tui.*
import tui.crossterm.Event
import tui.widgets.{BlockWidget, ClearWidget}

import scala.language.adhocExtensions

object FormulaPromptPopup:
  def apply(navigation: Navigation)(message: String, title: Option[String] = None)(
    confirm: Formula => Unit,
    dismiss: () => Unit
  ): FormulaPromptPopup =
    val model = FormulaInputModel(navigation) {
      new FormulaInputModel.Callbacks:
        override def onQuit(): Unit = dismiss()

        override def onSubmit(formula: Formula): Unit =
          confirm(formula)
    }

    model.edit()
    val formulaInput = new TextInput(model)(model)
    new FormulaPromptPopup(message, title)(formulaInput)

class FormulaPromptPopup(
  message: String,
  title: Option[String] = None
)(formulaInput: TextInput)
    extends Screen:

  private val ySize = 50
  private val xSize = 50

  override def headerText: Text =
    Text.nostyle(title.getOrElse(""))

  override def footerText: Text = formulaInput.footerText

  override def handleEvent(event: Event): Unit = formulaInput.handleEvent(event)

  override def render(renderer: Renderer, area: Rect): Unit =
    val contentArea = Rectangle(ySize, xSize, area)
    val background  = ClearWidget
    val border      = BlockWidget(title = title.map(Spans.nostyle), borders = Borders.ALL)

    renderer.render(background, contentArea)
    renderer.render(border, contentArea)
    formulaInput.render(renderer, contentArea)

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
*/