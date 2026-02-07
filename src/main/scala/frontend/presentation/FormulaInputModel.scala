package proofPlayground
package frontend.presentation

import core.logic.propositional.Formula
import frontend.notation.FormulaParser.parser
import frontend.tui.Navigation

import scala.util.Success

object FormulaInputModel:
  trait Data:
    def inputHandler: String => Either[Unit, String]

  trait Signals:
    def quit(): Unit

class FormulaInputModel(navigation: Navigation) extends FormulaInputModel.Data, FormulaInputModel.Signals:
  override def inputHandler: String => Either[Unit, String] =
    input =>
      Formula.parser.parse(input) match
        case Success(value) if value.remaining.isEmpty =>
          navigation.navigateTo(Navigation.Screen.ProofTree(value.parsed))
          Left(())
        case _ => Right("invalid formula")

  override def quit(): Unit =
    navigation.exit()
