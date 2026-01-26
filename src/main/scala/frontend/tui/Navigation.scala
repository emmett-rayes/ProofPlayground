package proofPlayground
package frontend.tui

import core.logic.propositional.Formula

object Navigation:
  enum Screen:
    case FormulaInput
    case ProofTree(formula: Formula)

trait Navigation:
  def exit(): Unit
  def navigateTo(screen: Navigation.Screen): Unit
  def showPopup(message: String, title: Option[String] = None)(callback: () => Unit): Unit
