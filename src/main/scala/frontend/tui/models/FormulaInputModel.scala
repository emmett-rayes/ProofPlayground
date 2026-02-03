package proofPlayground
package frontend.tui.models

import core.logic.propositional.Formula
import frontend.notation.FormulaParser.parser
import frontend.tui.Navigation

import scala.util.Success

class FormulaInputModel(navigation: Navigation):
  def inputHandler: String => Either[Unit, String] =
    input =>
      Formula.parser.parse(input) match
        case Success(value) if value.remaining.isEmpty =>
          navigation.navigateTo(Navigation.Screen.ProofTree(value.parsed))
          Left(())
        case _ => Right("invalid formula")

  def quit(): Unit =
    navigation.exit()
