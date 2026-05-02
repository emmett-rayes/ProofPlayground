package proofPlayground
package frontend.presentation

import scala.compiletime.uninitialized

import core.Functor
import core.logic.propositional.{Formula, FormulaF}
import core.logic.propositional.Formula.given
import core.meta.{MetaVariable, MetaVars, MapUnification, Pattern, Substitute, Unify}
import core.proof.{ClosedQuery, InferenceRule, Proof, ProofNode, ProofSystem, ProofRequirements, ProofZipper}
import core.proof.Assistant.ProofResult
import core.proof.ProofRequirements.given
import core.proof.ProofZipper.*
import core.proof.ProofZipper.given
import frontend.presentation.FormulaInputModel.ProofSystemChoice
import frontend.Show
import frontend.tui.Navigation
import zipper.Tree
import zipper.TreeZipper.given
import zipper.Zipper.root
import core.proof.Proof.*

object ProofTreeModel {
  trait Data {
    def proofTree: Tree[ProofStep]
    def rules: Vector[ProofRule]
    def focusOnRules: Boolean
    def isNodeSelected(node: Tree[ProofStep]): Boolean
  }

  trait Signals {
    def quit(): Unit
    def up(): Unit
    def down(): Unit
    def left(): Unit
    def right(): Unit
    def selectNode(): Unit
    def selectRule(index: Option[Int]): Unit
  }

  case class ProofStep(formula: String, labels: Seq[String])
  case class ProofRule(active: Boolean, label: String)

  def apply(navigation: Navigation)(
    formula: Formula,
    system: FormulaInputModel.ProofSystemChoice
  ): ProofTreeModel[?] = {
    import core.meta.Pattern.given
    import frontend.Show.given

    system match {
      case ProofSystemChoice.IntuitionisticNaturalDeduction =>
        import core.proof.natural
        import core.proof.natural.Judgement.given
        new ProofTreeModel(navigation)(
          ProofSystem.IntuitionisticPropositionalNaturalDeduction,
          natural.Judgement(formula, Seq.empty)
        )
      case ProofSystemChoice.ClassicalSequentCalculus =>
        import core.proof.sequent
        import core.proof.sequent.Judgement.given
        new ProofTreeModel(navigation)(
          ProofSystem.ClassicalPropositionalSequentCalculus,
          sequent.Judgement(Seq.empty, Seq(formula))
        )
    }
  }
}

class ProofTreeModel[
  J[_] <: AnyRef: {Functor, Substitute[Formula, FormulaF]}
](navigation: Navigation)(system: ProofSystem[J, FormulaF], judgement: J[Formula])(using
  J[Formula] is Show,
  J[Formula] is ClosedQuery,
  J[Pattern[FormulaF]] is Show,
  J[Pattern[FormulaF]] is MetaVars,
) extends ProofTreeModel.Data, ProofTreeModel.Signals {

  private given req: ProofRequirements[FormulaF, J] = summon

  private val inferenceRules = system.rules.toVector.sortBy(_.label)

  private var selected: ProofTreeModel.ProofStep = uninitialized

  private var rulesInFocus = false
  private var zipper       = Proof(judgement).zipper

  private var cachedRuleActive: Map[InferenceRule[J, FormulaF], Boolean] = Map.empty // helps spam the debugger less

  private def invalidateRuleCache(): Unit =
    cachedRuleActive = Map.empty

  override def focusOnRules: Boolean = rulesInFocus

  override def rules: Vector[ProofTreeModel.ProofRule] = inferenceRules.map { rule =>
    val active = cachedRuleActive.getOrElse(
      rule, {
        val computed = zipper.get.apply(rule) match {
          case ProofResult.UnificationFailure(_)  => false
          case ProofResult.Success(_)             => true
          case ProofResult.SubstitutionFailure(_) => true // substitution failures can be fixed by user input
        }
        cachedRuleActive = cachedRuleActive.updated(rule, computed)
        computed
      }
    )
    ProofTreeModel.ProofRule(active, rule.label)
  }

  override def proofTree: Tree[ProofTreeModel.ProofStep] =
    val proof = zipper.root.get
    def recur(p: Tree[ProofNode[FormulaF, J]]): Tree[ProofTreeModel.ProofStep] = {
      val node      = p.value
      val judgement = node.judgement
      var labels    = Seq.empty[String]
      if !judgement.closed then
        labels :+= node.rule.map(_.label).getOrElse("?")
      if !node.sidecondition(p) then
        labels :+= "!"
      if labels.isEmpty then
        labels :+= " "

      val result = ProofTreeModel.ProofStep(judgement.show, labels)
      // remember the current position for `isNodeSelected`
      if judgement eq zipper.get.conclusion then selected = result
      Tree(result, p.children.map(recur))
    }
    recur(proof)

  override def isNodeSelected(node: Tree[ProofTreeModel.ProofStep]): Boolean =
    selected eq node.value

  override def up(): Unit =
    zipper = zipper.down.getOrElse(zipper) // proof trees are upside down
    invalidateRuleCache()

  override def down(): Unit =
    zipper = zipper.up.getOrElse(zipper) // proof trees are upside down
    invalidateRuleCache()

  override def left(): Unit =
    zipper = zipper.left.getOrElse(zipper)
    invalidateRuleCache()

  override def right(): Unit =
    zipper = zipper.right.getOrElse(zipper)
    invalidateRuleCache()

  override def selectNode(): Unit =
    rulesInFocus = true

  override def selectRule(index: Option[Int]): Unit = {
    if index.isEmpty then rulesInFocus = false
    for
      idx  <- index
      rule <- inferenceRules.lift(idx)
    yield applyRule(rule) { substitutedRule =>
      handleMissingMetaVariables(substitutedRule, substitutedRule.metavariables.toSeq) { unification =>
        applyRule(substitutedRule, unification) { _ =>
          navigation.showPopup(Navigation.Popup.Confirm(
            "The supplied meta-variable substitutions are not sufficient for applying the rule.",
            title = Some("Substitution Error"),
            hasConfirm = false,
          )) { () => () }
        }
      }
    }
  }

  override def quit(): Unit =
    navigation.showPopup(Navigation.Popup.Confirm("Do you want to quit the proof mode?", Some("Quit"))) { () =>
      navigation.navigateTo(Navigation.Screen.FormulaInput)
    }

  private def handleMissingMetaVariables(
    rule: InferenceRule[J, FormulaF],
    metavariables: Seq[MetaVariable],
    unification: req.Uni[Formula] = req.Uni.empty
  )(
    callback: req.Uni[Formula] => Unit
  ): Unit = {
    recur(metavariables, unification)

    def recur(metavariables: Seq[MetaVariable], unification: req.Uni[Formula]): Unit = {
      if metavariables.isEmpty then
        callback(unification)
      else
        navigation.showPopup(Navigation.Popup.MissingMetaVariable(metavariables.head, rule)) {
          formula =>
            val updated = unification.update(Map(metavariables.head -> formula))
            recur(metavariables.tail, updated.get)
        }
    }
  }

  private def applyRule(
    rule: InferenceRule[J, FormulaF],
    unification: req.Uni[Formula] = req.Uni.empty,
  )(substitutionFailure: InferenceRule[J, FormulaF] => Unit): Unit = {
    def replace(replacement: Proof[FormulaF, J]): Unit = {
      rulesInFocus = false
      zipper = zipper.replace(replacement)
      zipper = zipper.down.getOrElse(zipper)
      invalidateRuleCache()
    }

    zipper.get.apply(rule, unification) match {
      case ProofResult.UnificationFailure(_)                => ()
      case ProofResult.Success(proof)                       => replace(proof)
      case ProofResult.SubstitutionFailure(substitutedRule) => substitutionFailure(substitutedRule)
    }
  }
}
