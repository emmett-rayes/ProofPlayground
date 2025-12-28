package proofPlayground
package core.meta

import core.logic.propositional.FormulaF.{/\, variable}
import core.logic.propositional.{Formula, FormulaF}

import org.scalatest.funsuite.AnyFunSuite

class TestUnification extends AnyFunSuite:

  test("meta-variables unify any propositional formula") {
    val metavariable = Pattern.Formula.Meta[FormulaF]("phi")
    val pattern = metavariable
    val formula = Formula(Formula(variable()) /\ Formula(variable()))
    val result = Unification.unify(pattern, formula)

    assert(result.isDefined)
    assert(result.get === Map(metavariable -> formula))
  }
