package proofPlayground
package frontend.presentation

import core.logic.propositional.{Formula, FormulaF}
import core.meta.MetaVariable
import core.proof.InferenceRule
import core.proof.natural.Judgement
import frontend.Show.given
import frontend.notation.FormulaParser.parser

import scala.util.Success

object MissingMetaVariableModel:
  type InferenceRuleString = (label: String, conclusion: String, premises: Vector[String])

  trait Data:
    def variable: String
    def inferenceRule: InferenceRuleString
    def inputHandler: String => Either[Unit, String]

  trait Signals:
    def exit(): Unit

class MissingMetaVariableModel(confirm: Formula => Unit, dismiss: () => Unit)(
  metavariable: MetaVariable,
  rule: InferenceRule[Judgement, FormulaF]
) extends MissingMetaVariableModel.Data, MissingMetaVariableModel.Signals:
  override def variable: String = metavariable.name

  override def inferenceRule: MissingMetaVariableModel.InferenceRuleString =
    (rule.label, rule.conclusion.show, rule.premises.map(_.show).toVector)

  override def inputHandler: String => Either[Unit, String] =
    input =>
      Formula.parser.parse(input) match
        case Success(value) if value.remaining.isEmpty =>
          dismiss()
          confirm(value.parsed); Left(())
        case _ => Right("invalid formula")

  override def exit(): Unit =
    dismiss()
