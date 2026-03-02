package proofPlayground
package frontend.presentation

import scala.util.Success

import core.logic.propositional.Formula
import frontend.notation.FormulaParser.parser
import frontend.tui.Navigation

object FormulaInputModel {

  enum ProofSystemChoice(val label: String) {
    case IntuitionisticNaturalDeduction extends ProofSystemChoice("Intuitionistic Natural Deduction")
    case ClassicalSequentCalculus       extends ProofSystemChoice("Classical Sequent Calculus")
  }

  trait Data {
    def inputHandler(system: Option[FormulaInputModel.ProofSystemChoice]): String => Either[Unit, String]
  }

  trait Signals {
    def quit(): Unit
  }
}

class FormulaInputModel(navigation: Navigation) extends FormulaInputModel.Data, FormulaInputModel.Signals {
  override def inputHandler(system: Option[FormulaInputModel.ProofSystemChoice]): String => Either[Unit, String] =
    input =>
      system match {
        case None         => Right("no proof system selected")
        case Some(system) =>
          Formula.parser.parse(input) match {
            case Success(value) if value.remaining.isEmpty =>
              navigation.navigateTo(Navigation.Screen.ProofTree(value.parsed, system))
              Left(())
            case _ => Right("invalid formula")
          }
      }

  override def quit(): Unit =
    navigation.exit()
}
