package proofPlayground
package core.proof

import core.Functor
import core.Fix
import core.meta.{FreeVars, MapUnification, MetaVariable, MetaVars, Unify}
import zipper.Tree

case class SideCondition[T, J[_]](
  metajudgement: Option[J[T]],
  condition: [F[_]] => (Fix[F] is FreeVars) ?=> (J[Fix[F]], Proof[F, J]) => Boolean,
)

object SideCondition {

  def apply[T, J[_]](metajudgement: J[T])(
    condition: [F[_]] => (Fix[F] is FreeVars) ?=> (J[Fix[F]], Proof[F, J]) => Boolean
  ): SideCondition[T, J] = SideCondition(Some(metajudgement), condition)

  given [J[_]: Functor] => Functor[[T] =>> SideCondition[T, J]] {
    extension [A](sidecondition: SideCondition[A, J]) {
      override def map[B](f: A => B): SideCondition[B, J] =
        sidecondition.copy(metajudgement = sidecondition.metajudgement.map(_.map(f)))
    }
  }
}
