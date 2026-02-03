package proofPlayground
package frontend.tui

import core.logic.propositional.{Formula, FormulaF}
import core.meta.MetaVariable
import core.proof.InferenceRule
import core.proof.natural.Judgement

object Navigation:
  sealed trait Popup:
    type Callback

  enum Screen:
    case FormulaInput
    case ProofTree(formula: Formula)

  object Popup:
    case class Confirm(message: String, title: Option[String] = None) extends Popup:
      override type Callback = () => Unit

    case class MissingMetaVariable(metavariable: MetaVariable, rule: InferenceRule[Judgement, FormulaF]) extends Popup:
      override type Callback = String => Either[Unit, String]

trait Navigation:
  def exit(): Unit
  def navigateTo(screen: Navigation.Screen): Unit
  def showPopup(popup: Navigation.Popup)(callback: popup.Callback): Unit
