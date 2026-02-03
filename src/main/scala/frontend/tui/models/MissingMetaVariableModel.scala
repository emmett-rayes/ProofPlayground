package proofPlayground
package frontend.tui.models

import core.logic.propositional.{Formula, FormulaF}
import core.meta.MetaVariable
import core.proof.InferenceRule
import core.proof.natural.Judgement
import frontend.notation.FormulaParser.parser
import frontend.Show.given

import scala.util.{Failure, Success}

object MissingMetaVariableModel:
  type InferenceRuleString = (label: String, conclusion: String, hypotheses: Vector[String])

  trait Data:
    def variable: String
    def inferenceRule: InferenceRuleString

    def inputHandler: String => Either[Unit, String]
    def exitHandler: () => Unit

class MissingMetaVariableModel(confirm: Formula => Unit, dismiss: () => Unit)(
  metavariable: MetaVariable,
  rule: InferenceRule[Judgement, FormulaF]
) extends MissingMetaVariableModel.Data:
  override def variable: String = metavariable.name

  override def inferenceRule: MissingMetaVariableModel.InferenceRuleString =
    (rule.label, rule.conclusion.show, rule.hypotheses.map(_.show).toVector)

  override def inputHandler: String => Either[Unit, String] =
    input =>
      Formula.parser.parse(input) match
        case Failure(_)     => Right("invalid formula")
        case Success(value) =>
          confirm(value.parsed); Left(())

  override def exitHandler: () => Unit = dismiss
