package proofPlayground
package frontend.tui

import core.logic.propositional.{Formula, FormulaF}
import core.meta.{MetaVariable, Pattern}
import core.proof.InferenceRule
import frontend.Show

object Navigation {
  sealed trait Popup {
    type Callback
  }

  enum Screen {
    case FormulaInput
    case ProofTree(formula: Formula)
  }

  object Popup {
    case class Confirm(message: String, title: Option[String] = None) extends Popup {
      override type Callback = Option[() => Unit]
    }

    case class MissingMetaVariable[J[_]](metavariable: MetaVariable, rule: InferenceRule[J, FormulaF])(using
      val show: J[Pattern[FormulaF]] is Show
    ) extends Popup {
      override type Callback = Formula => Unit
    }
  }
}

trait Navigation {
  def exit(): Unit
  def navigateTo(screen: Navigation.Screen): Unit
  def showPopup(popup: Navigation.Popup)(callback: popup.Callback): Unit
}
